# lipstick
Color and Color Space PHP classes named after a pretty Pink Flowering Strawberry

# How to install

`composer require esmero/lipstick:dev-main`

# Basic usage
```PHP
<?PHP

require 'vendor/autoload.php';

use \Esmero\Lipstick\Color;

$prettycolor = new Color('#5588CC');
print_r($prettycolor->getRgb());
print_r($prettycolor->getXyz());
print_r($prettycolor->getCielab());
print_r($prettycolor->getCam16());
```

## Output will be

```
Array
(
    [R] => 85
    [G] => 136
    [B] => 204
)
Array
(
    [X] => 0.23446234045762
    [Y] => 0.23897966766939
    [Z] => 0.60496347657347
)
Array
(
    [L] => 55.985491376125
    [a] => 312.9593458713
    [b] => 123.29081923678
)
Array
(
    [J] => 45.54426472036
    [C] => 45.070010482938
    [h] => 259.22534529813
    [Q] => 132.96974182692
    [M] => 39.41306078701
    [s] => 54.443203141326
)
```

# Acknowledgments

This (WIP) Library uses Matrices & Math & ways translated from https://observablehq.com/@jrus/cam16 authored by https://github.com/jrus. Many thanks.
