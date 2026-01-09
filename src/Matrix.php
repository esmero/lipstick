<?php
namespace Esmero\Lipstick;
use InvalidArgumentException;

class Matrix {

  private int $rows;

  private int $cols;

  private array $data;

  public function __construct(array $array) {
    $cols = FALSE;
    $vector = FALSE;
    $matrix = FALSE;
    if (empty($array)) {
      throw new InvalidArgumentException('Matrix data supplied is empty');
    }

    $rowcount = count($array);
    foreach ($array as $rowcount => $row) {
      if (!is_array($row)) {
        if (is_numeric($row)) {
          $vector = true;
        }
        else {
          throw new InvalidArgumentException('Matrix data supplied has a non numeric component');
        }
        if ($matrix) {
          throw new InvalidArgumentException('Numeric and Array elements can not be mixed in a Vector');
        }
      }
      else {
        if ($vector) {
          // means we have an array but already detected as a vector before, so we return FALSE;
          throw new InvalidArgumentException('Matrix data supplied is inconsistent. Every Row has to have the same Column count');
        }
        else {
          $cols = !$cols ? count($row) : ($cols == count($row) ? $cols: FALSE );
          if (!$cols) {
            // means a previous colum count differed from the current one
            // Matrix is incomplete/unbalanced.
            throw new InvalidArgumentException('Matrix data supplied is inconsistent. Every Row has to have the same Column count');
          }
          else {
            foreach ($row as $element) {
              if (!is_numeric($element)) {
                // we check for each row component. If not numeric, we bail out
                throw new InvalidArgumentException('Matrix data supplied has a non numeric component');
              }
            }
            $matrix = TRUE;
          }
        }
      }
    }
    $this->rows = $vector ? 1 : $rowcount + 1;
    $this->cols = $vector ? $rowcount + 1 : $cols + 1;
    $this->data = $vector ? [$array] : $array;
  }

  public function getData(): array {
    return $this->data;
  }

  public function getRowCount(): int {
    return $this->rows;
  }

  public function getColCount(): int {
    return $this->cols;
  }

}