<?php

class MathHelper {
  public static function lerp( $a,  $b,  $t): float {
    // linear interpolation;
    return (float) ($a + ($b - $a) * $t);
  }

  public static function clip(float $a, float $b, float $t): float {
    return (float) min(max($t, $a), $b);
  }

  /**
   * @param $array1
   * @param $array2
   * @param callable $callable
   *    A Callable that takes at least 2 arguments, one per matrix element.
   * @param ...$arguments
   *    Any Extra arguments the Callable needs.
   *
   * @return array
   *    Always nested as a matrix (at least one row each being an array of numbers), even if the input was two vectors
   * @throws \InvalidArgumentException
   *    If the $arrays can't be converted to matrices
   */
  public static function matrix_elem_op(array|Matrix $arrayOrMatrix1, array|Matrix $arrayOrMatrix2, bool $return_as_matrix, callable $callable, ...$arguments) {
    // Check if $array1 and $array 2 are compatible.
    // $array1 can be a vector or a matrix.
    // $array2 can be a compatible vector or matrix compared to $array1
    if (is_array($arrayOrMatrix1)) {
      $arrayOrMatrix1 = new Matrix($arrayOrMatrix1);
    }
    if (is_array($arrayOrMatrix2)) {
      $arrayOrMatrix2 = new Matrix($arrayOrMatrix2);
    }

    $opresults = [];
    // Both are matrices and same dimensions
    if (($arrayOrMatrix1->getRowCount() == $arrayOrMatrix2->getRowCount) && ($arrayOrMatrix2->getColCount == $arrayOrMatrix2->getColCount)) {
      for ($i = 0; $i < $arrayOrMatrix1->getRowCount(); $i++) {
        for ($j = 0; $j < $arrayOrMatrix1->getColCount(); $j++) {
          $argument_local = $arguments;
          is_array($argument_local) && !empty($argument_local) ? array_unshift($argument_local, $arrayOrMatrix2->getData[$i][$j]) :  $argument_local = [$arrayOrMatrix2->getData[$i][$j]];
          array_unshift($argument_local, $arrayOrMatrix1->getData()[$i][$j]);
          $opresults[$i][$j] = call_user_func_array($callable, $argument_local);
        }
      }
    }
    else {
      throw new InvalidArgumentException('Arrays passed are not compatible for an element to element operation.');
    }
    if ($return_as_matrix) {
      $opresults = new Matrix($opresults);
    }
    return $opresults;
  }
}