<?php
declare(strict_types=1);

namespace Esmero\Lipstick;

use InvalidArgumentException;
use LogicException;
use SplObjectStorage;

class VectorSpace extends SplObjectStorage {

    /**
     * @var int
     */
    protected int $dimension;

    public function __construct(int $dimension) {
        if ($dimension < 1) {
            throw new LogicException('a Vector space dimension needs to larger or equal than 1');
        }

        $this->dimension = $dimension;
    }

    public function toArray(): array {
        $vectors = [];

        /** @var Vector $vector */
        foreach ($this as $vector) {
            $vectors[] = $vector->toArray();
        }

        return $vectors;
    }

    /**
     * @param mixed $label
     */
    public function newVector(array $array, ?string $label = NULL): Vector {
        if (count($array) !== $this->dimension) {
            throw new LogicException('(' . implode(',', $array) . ') is not a valid vector for this Vector space');
        }

        return new Vector($array, $label);
    }

    /**
     * @param mixed $label
     * @param mixed $data
     */
    public function addVector(array $array, $label = NULL, $data = NULL): void {
        $this->attach($this->newVector($array, $label), $data);
    }

    /**
     * @param object $object
     * @param mixed $data
     */
    public function attach($object, $info = NULL): void {
        if (!$object instanceof Vector) {
            throw new InvalidArgumentException('can only attach Vector type Objects to Vector spaces');
        }

        parent::attach($object, $info);
    }

    public function getDimension(): int {
        return $this->dimension;
    }

    /**
     * @return array|bool
     */
    public function getBoundaries(): array|bool {
        if (count($this) === 0) {
            return FALSE;
        }

        $min = $this->newVector(array_fill(0, $this->dimension, NULL));
        $max = $this->newVector(array_fill(0, $this->dimension, NULL));

        /** @var Vector $vector */
        foreach ($this as $vector) {
            for ($n = 0; $n < $this->dimension; ++$n) {
                if ($min[$n] === NULL || $min[$n] > $vector[$n]) {
                    $min[$n] = $vector[$n];
                }

                if ($max[$n] === NULL || $max[$n] < $vector[$n]) {
                    $max[$n] = $vector[$n];
                }
            }
        }

        return [$min, $max];
    }

    public function getRandomVector(Vector $min, Vector $max): Vector {
        $vector = $this->newVector(array_fill(0, $this->dimension, NULL));

        for ($n = 0; $n < $this->dimension; ++$n) {
            $vector[$n] = random_int($min[$n], $max[$n]);
        }

        return $vector;
    }


}
