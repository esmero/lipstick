<?php
declare(strict_types=1);

namespace Esmero\Lipstick;

use ArrayAccess;
use Countable;
use LogicException;

class Vector implements ArrayAccess, Countable
{
    /**
     * @var int
     */
    protected int $dimension;

    /**
     * @var array
     */
    protected array $vector = [];

    /**
     * @var mixed
     */
    protected mixed $label;

    /**
     * @var int
     */
    protected int $weight = 1;

    /**
     * @param array $data
     * @param int $weight
     * @param mixed $label
     */
    public function __construct(array $data, int $weight  = 1, mixed $label = null)
    {
        $this->dimension = count($data);
        $this->vector = $data;
        $this->label = $label;
        $this->weight = $weight;
    }

    public function toArray(): array
    {
        return $this->vector;
    }


    private function euclideanDistance(self $vector, ?self $featuresWeight):float {
        $sum = 0.0;
        for ($i = 0; $i < $this->dimension; $i++) {
            $diff =  $this->vector[$i] - $vector[$i];
            $sum += ($featuresWeight ? $featuresWeight[$i] : 1) * ($diff * $diff);
        }
        return sqrt($sum);
    }

    private function manhattanDistance(self $vector):float {
        $sum = 0.0;
        for ($i = 0; $i < $this->dimension; $i++) {
            $sum += abs( $this->vector[$i] -  $vector[$i]);
        }
        return $sum;
    }


    private function cosineDistance(self $vector): float {
        $dotProduct = 0.0;
        $magnitudeA = 0.0;
        $magnitudeB = 0.0;

        for ($i = 0; $i < $this->dimension; $i++) {
            $dotProduct += $this->vector[$i] * $vector[$i];
            $magnitudeA += $this->vector[$i] * $this->vector[$i];
            $magnitudeB += $vector[$i] * $vector[$i];
        }
        // L2 Norms
        $magnitudeA = sqrt($magnitudeA);
        $magnitudeB = sqrt($magnitudeB);

        if ($magnitudeA == 0.0 || $magnitudeB == 0.0) {
            // vectors are orthogonal and dissimilar
            return 1.0;
        }
        $similarity = $dotProduct / ($magnitudeA * $magnitudeB);
        return  1.0 - $similarity;
    }

    /**
     * @param \Esmero\Lipstick\Vector $vector
     * @param int|float $p
     *      $p = 1 ==> Manhattan distance
     *      $p = 2 ==> Euclidean Distance
     *
     * @see https://en.wikipedia.org/wiki/Minkowski_distance
     *
     * @return float|int
     */
    public function minkowskiDistance(self $vector, int|float $p = 1): float|int {
        if ($p < 1) {
            throw new LogicException("p must be equal or larger than 1.0");
        }

        $sum = array_sum(
            array_map(
                function ($x, $y) use ($p) {
                    return abs($x - $y) ** $p;
                },
                $this->toArray(),
                $vector->toArray()
            )
        );

        return $sum ** (1 / $p);
    }

    /**
     * @param \Esmero\Lipstick\Vector $vector
     * @param string $metric
     * @param \Esmero\Lipstick\Vector|null $featuresWeight
     *
     * @return float|int
     */
    public function distanceTo(self $vector, string $metric, ?self $featuresWeight = null): float|int {
        if ($featuresWeight && $this->dimension !== $featuresWeight->dimension) {
            throw new LogicException('When passing a per Feature Weight vector as argument it needs to match this vectors dimension');
        }
        if ($vector->getDimension()!= $this->dimension) {
            throw new LogicException('This vector and passed Vector space dimension need to match');
        }

        return match ($metric) {
            "manhattan" => $this->manhattanDistance($vector),
            "cosine" => $this->cosineDistance($vector),
            default => $this->euclideanDistance($vector, $featuresWeight),
        };
    }

    /**
     * @param \Esmero\Lipstick\Vector $vector
     * @param callable $callable
     * @param mixed ...$arguments
     *
     * @return float|int
     */
    public function distanceToWithCallback(self $vector, callable $callable, ...$arguments): float|int {
        if ($vector->getDimension()!= $this->dimension) {
            throw new LogicException('This vector and passed Vector space dimension need to match');
        }
        if (!is_callable($callable)) {
            throw new LogicException('The passed callable for Distance calculation is not valid');
        }
        // @TODO use reflection function to check if the distance callback takes two arrays (required) arguments
        $argument_local = $arguments;
        (is_array($argument_local) && !empty($argument_local)) ? array_unshift($argument_local,$vector->toArray()) :  $argument_local = [$vector->toArray()];
        array_unshift($argument_local, $this->toArray());
        return call_user_func_array($callable, $argument_local);
    }

    /**
     * @param \Esmero\Lipstick\VectorSpace $space
     * @param string $metric
     *
     * @return \Esmero\Lipstick\Vector|null
     */
    public function getClosest(VectorSpace $space, string $metric = 'euclidean' ): ?self
    {
        $minPoint = null;
        if ($space->getDimension()!= $this->dimension) {
            throw new LogicException('This vector and passed Vector space dimension need to match');
        }

        foreach ($space as $vector) {
            $distance = $this->distanceTo($vector, $metric);

            if (!isset($minDistance)) {
                $minDistance = $distance;
                $minPoint = $vector;
                continue;
            }

            if ($distance < $minDistance) {
                $minDistance = $distance;
                $minPoint = $vector;
            }
        }

        return $minPoint;
    }

    /**
     * @return int
     */
    public function getSampleWeight(): int {
        return $this->weight;
    }

    public function getLabel(): int {
        return $this->label;
    }


    public function getVector(): array
    {
        return $this->vector;
    }

    /**
     * @param mixed $offset
     */
    public function offsetExists($offset): bool
    {
        return isset($this->vector[$offset]);
    }

    /**
     * @param mixed $offset
     *
     * @return mixed
     */
    public function offsetGet($offset): mixed {
        return $this->vector[$offset];
    }

    /**
     * @param mixed $offset
     * @param mixed $value
     */
    public function offsetSet($offset, $value): void
    {
        $this->vector[$offset] = $value;
    }

    /**
     * @param mixed $offset
     */
    public function offsetUnset($offset): void
    {
        unset($this->vector[$offset]);
    }

    public function count(): int
    {
        return count($this->vector);
    }

    public function getDimension(): int {
        return $this->dimension;
    }

}
