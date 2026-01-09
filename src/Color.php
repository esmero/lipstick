<?php
namespace Esmero\Lipstick;

class Color {

  private string $hex;
  private array $srgb;

  private array $xyz;

  private array $cam16;

  private array $cielab;

  private array $rgb;

  private string $whitepoint;

  public const WHITEPOINTS = [
    "A" =>   ['X' => 109.850, 'Y' => 100, 'Z' => 35.585],
    "B" =>   ['X' => 99.090, 'Y' => 100, 'Z' => 85.324],
    "C" =>   ['X' => 98.074, 'Y' => 100, 'Z' => 118.232],
    "E" =>   ['X' => 100, 'Y' => 100, 'Z' => 100], // equal-energy illuminant
    "D50" => ['X' => 96.422, 'Y' => 100, 'Z' => 82.521],
    "D55" => ['X' => 95.682, 'Y' => 100, 'Z' => 92.149],
    "D65"=> ['X' => 95.047, 'Y' => 100, 'Z' => 108.883],
    "D75" => ['X' => 94.972, 'Y' => 100, 'Z' => 122.638],
    "F2" =>  ['X' => 99.186, 'Y' => 100,  'Z' => 67.393],
    "F7" =>  ['X' => 95.041, 'Y' => 100, 'Z' => 108.747],
    "F11" => ['X' => 100.962, 'Y' => 100,  'Z' => 64.350],
  ];


  public function __construct(string $hex, $whitepoint = "D65")
  {
    $this->hex = $hex;
    if (array_key_exists($whitepoint,static::WHITEPOINTS )) {
      $this->whitepoint = $whitepoint;
    }
    else {
      $this->whitepoint = "D65";
    }
    $this->rgb = $this->hexToRgb($hex);
  }

  /**
   * Static Constructor if you only have an Integer representation
   *
   * @param int $integer
   * @param string $whitepoint
   *
   * @return self
   */
  public static function newFromInt(int $integer, string $whitepoint = "D65"):Color
  {
    $hex_color = sprintf("#%02x%02x%02x", ($integer >> 16) & 0xFF, ($integer >> 8) & 0xFF,  $integer & 0xFF);
    if (!array_key_exists($whitepoint,static::WHITEPOINTS )) {
      $whitepoint = "D65";
    }
    return new self($hex_color, $whitepoint);
  }

  /**
   * @param \Esmero\Lipstick\Color $OtherColorObject
   *
   * @return float
   */
  public function ciede2000DeltaE(color $OtherColorObject):float
  {
    $firstLabColor = $this->getCielab();
    $secondLabColor = $OtherColorObject->getCielab();

    $C1 = sqrt(pow($firstLabColor['a'], 2) + pow($firstLabColor['b'], 2));
    $C2 = sqrt(pow($secondLabColor['a'], 2) + pow($secondLabColor['b'], 2));
    $Cb = ($C1 + $C2) / 2;

    $G = .5 * (1 - sqrt(pow($Cb, 7) / (pow($Cb, 7) + pow(25, 7))));

    $a1p = (1 + $G) * $firstLabColor['a'];
    $a2p = (1 + $G) * $secondLabColor['a'];

    $C1p = sqrt(pow($a1p, 2) + pow($firstLabColor['b'], 2));
    $C2p = sqrt(pow($a2p, 2) + pow($secondLabColor['b'], 2));

    $h1p = $a1p == 0 && $firstLabColor['b'] == 0 ? 0 : atan2($firstLabColor['b'], $a1p);
    $h2p = $a2p == 0 && $secondLabColor['b'] == 0 ? 0 : atan2($secondLabColor['b'], $a2p);

    $LpDelta = $secondLabColor['L'] - $firstLabColor['L'];
    $CpDelta = $C2p - $C1p;

    if ($C1p * $C2p == 0) {
      $hpDelta = 0;
    } elseif (abs($h2p - $h1p) <= 180) {
      $hpDelta = $h2p - $h1p;
    } elseif ($h2p - $h1p > 180) {
      $hpDelta = $h2p - $h1p - 360;
    } else {
      $hpDelta = $h2p - $h1p + 360;
    }

    $HpDelta = 2 * sqrt($C1p * $C2p) * sin($hpDelta / 2);

    $Lbp = ($firstLabColor['L'] + $secondLabColor['L']) / 2;
    $Cbp = ($C1p + $C2p) / 2;

    if ($C1p * $C2p == 0) {
      $hbp = $h1p + $h2p;
    } elseif (abs($h1p - $h2p) <= 180) {
      $hbp = ($h1p + $h2p) / 2;
    } elseif ($h1p + $h2p < 360) {
      $hbp = ($h1p + $h2p + 360) / 2;
    } else {
      $hbp = ($h1p + $h2p - 360) / 2;
    }

    $T = 1 - .17 * cos($hbp - 30) + .24 * cos(2 * $hbp) + .32 * cos(3 * $hbp + 6) - .2 * cos(4 * $hbp - 63);

    $sigmaDelta = 30 * exp(-pow(($hbp - 275) / 25, 2));

    $Rc = 2 * sqrt(pow($Cbp, 7) / (pow($Cbp, 7) + pow(25, 7)));

    $Sl = 1 + ((.015 * pow($Lbp - 50, 2)) / sqrt(20 + pow($Lbp - 50, 2)));
    $Sc = 1 + .045 * $Cbp;
    $Sh = 1 + .015 * $Cbp * $T;

    $Rt = -sin(2 * $sigmaDelta) * $Rc;

    return sqrt(
      pow($LpDelta / $Sl, 2) +
      pow($CpDelta / $Sc, 2) +
      pow($HpDelta / $Sh, 2) +
      $Rt * ($CpDelta / $Sc) * ($HpDelta / $Sh)
    );
  }

  /**
   * Provides Gamma Adjustment
   *
   * @param int|float $value
   *
   * @return float
   */
  private function rgbToSrgbStep(int|float $value): float {
    $value /= 255;
    return $value <= .03928 ? $value / 12.92 : pow(($value + .055) / 1.055, 2.4);
  }

  /**
   * @param array $rgb
   *
   * @return array
   */
  private function rgbToSrgb(array $rgb): array {
    return [
      'R' => $this->rgbToSrgbStep($rgb['R']),
      'G' => $this->rgbToSrgbStep($rgb['G']),
      'B' => $this->rgbToSrgbStep($rgb['B']),
    ];
  }

  /**
   * @param string $hex
   *
   * @return array{R: mixed, G: mixed, B: mixed}
   */
  protected function hexToRgb(string $hex):array {
    [$r, $g, $b] = array_map(
      function($c) {
        return hexdec(str_pad($c, 2, $c));
      },
      str_split(ltrim($hex, '#'), strlen($hex) > 4 ? 2 : 1)
    );
    return [
      'R' => $r,
      'G' => $g,
      'B' => $b,
    ];
  }

  /**
   * @param array $rgb
   *
   * @return array{X: float, Y: float, Z: float}
   */
  private function srgbToXyz(array $rgb):array {
    $div = 100;
    return [
      'X' => ((41.23865632529916 * $rgb['R']) + (35.75914909206253 * $rgb['G']) + (18.045049120356364 * $rgb['B']))/$div,
      'Y' => ((21.26368216773238 * $rgb['R']) + (71.51829818412506 * $rgb['G']) + (7.218019648142546 * $rgb['B']))/$div,
      'Z' => ((1.9330620152483982 * $rgb['R']) + (11.919716364020843 * $rgb['G']) + (95.03725870054352 * $rgb['B']))/$div,
    ];
  }

  /**
   * @param float $value
   *
   * @return float
   */
  private function xyzToLabStep(float $value):float
  {
    return $value > 216 / 24389 ? pow($value, 1 / 3) : 841 * $value / 108 + 4 / 29;
  }

  /**
   * @param array $xyz
   *
   * @return array
   */
  private function xyzToLab(array $xyz):array {
    //http://en.wikipedia.org/wiki/Illuminant_D65#Definition
    $Xn = static::WHITEPOINTS[$this->whitepoint]['X']/100;
    $Yn = static::WHITEPOINTS[$this->whitepoint]['Y']/100;
    $Zn = static::WHITEPOINTS[$this->whitepoint]['Z']/100;

    // http://en.wikipedia.org/wiki/Lab_color_space#CIELAB-CIEXYZ_conversions
    return [
      'L' => 116 * $this->xyzToLabStep($xyz['Y'] / $Yn) - 16,
      'a' => 500 * $this->xyzToLabStep($xyz['X'] / $Xn) - $this->xyzToLabStep($xyz['Y'] / $Yn),
      'b' => 200 * $this->xyzToLabStep($xyz['Y'] / $Yn) -$this->xyzToLabStep($xyz['Z'] / $Zn),
    ];
  }

  private function M16(array $xyz):array {
    // Calculates cone response
    return [
      + 0.401288*$xyz['X'] + 0.650173*$xyz['Y'] - 0.051461*$xyz['Z'],
      - 0.250268*$xyz['X'] + 1.204414*$xyz['Y'] + 0.045854*$xyz['Z'],
      - 0.002079*$xyz['X'] + 0.048952*$xyz['Y'] + 0.953127*$xyz['Z']
    ];
  }

  public function getWhitepoint(): string {
    return $this->whitepoint;
  }

  public function setWhitepoint(string $whitepoint): void {
    if ($whitepoint !== $this->whitepoint) {
      if (array_key_exists($whitepoint,static::WHITEPOINTS )) {
        $this->xyz = [];
        $this->cielab = [];
        $this->cam16 = [];
        $this->whitepoint = $whitepoint;
      }
    }
  }

  /**
   * @param array $RGB
   *
   * @return float[]
   */
  private function M16_inv(array $RGB):array {
    return [
      + 1.862067855087233e+0*$RGB['R'] - 1.011254630531685e+0*$RGB['G'] + 1.491867754444518e-1*$RGB['B'],
      + 3.875265432361372e-1*$RGB['R'] + 6.214474419314753e-1*$RGB['G'] - 8.973985167612518e-3*$RGB['B'],
      - 1.584149884933386e-2*$RGB['R'] - 3.412293802851557e-2*$RGB['G'] + 1.049964436877850e+0*$RGB['B']
    ];
  }

  /**
   * @param array $xyz
   *
   * @return array{J: float|int, C: float|int, h: float|int, Q: float, M: float|int, s: float|int}
   */
  private function xyzToCam16(array $xyz):array {
    // Our XYZ is already capped to 0-1, but here we use all * 100.
    $xyz['X'] = $xyz['X']*100;
    $xyz['Y'] = $xyz['Y']*100;
    $xyz['Z'] = $xyz['Z']*100;
    $L_A = 40; // adapting luminance
    $k = 1 / (5*$L_A + 1);
    $Y_b = 20 ; // background luminance;
    $Y_w = static::WHITEPOINTS[$this->whitepoint]['Y'];
    $n = $Y_b / $Y_w;
    $z = 1.48 + sqrt($n);
    $N_bb = 0.725 * pow($n, -0.2);
    $N_cb = $N_bb;
    $surround = 2; // means average
    $c = MathHelper::lerp(0.59, 0.69, $surround - 1);
    $F = ($c >= 0.59) ? MathHelper::lerp(0.9, 1.0, ($c - 0.59)/.1) :  MathHelper::lerp(0.8, 0.9, ($c - 0.525)/0.065);
    $N_c = $F;
    $F_L = pow($k, 4) * $L_A + 0.1 * (1 - pow($k, 4))*(1 - pow($k, 4)) * pow(5 * $L_A, 1/3);
    $F_L_4 = pow($F_L, 0.25);
    // Illuminant discounting (adaptation). Would be 1 if fully adapted
    $D = MathHelper::clip(0, 1, $F * (1 - 1 / 3.6 * exp((-$L_A - 42)/92)));
    $RGB_w = $this->M16(static::WHITEPOINTS[$this->whitepoint]); // Cone responses of the white point
    $D_RGB = array_map( function($C_w) use ($Y_w, $D) { return MathHelper::lerp(1, $Y_w/$C_w, $D);}, $RGB_w);
    $func_mult = function($n1, $n2) {
      return $n1 * $n2;
    };
    $adapt = function($component) use ($F_L) {
      $x = pow($F_L * abs($component) * 0.01, 0.42);
      return ($component <=> 0) * 400 * $x / ($x + 27.13);
    };
    $RGB_cw = [$RGB_w[0]*$D_RGB[0], $RGB_w[1]*$D_RGB[1], $RGB_w[2]*$D_RGB[2]];
    $RGB_aw = array_map($adapt, $RGB_cw);
    $A_w = $N_bb * (2*$RGB_aw[0] + $RGB_aw[1] + 0.05*$RGB_aw[2]);
    $matrix = MathHelper::matrix_elem_op($this->M16($xyz), $D_RGB, FALSE, $func_mult);
    [$R_a, $G_a, $B_a] = array_map($adapt, $matrix[0]);
    $a = $R_a + (-12*$G_a + $B_a) / 11;         // redness-greenness
    $b = ($R_a + $G_a - 2 * $B_a) / 9;           // yellowness-blueness
    $h_rad = atan2($b, $a);    // hue in radians
    // php might return a negative, we need to positive, Can't
    // use the % (modulus) operator bc we loose floating point precision.
    $h = fmod(rad2deg($h_rad),360);
    $h = $h < 0 ? $h + 360 : $h;
    $e_t = 0.25 * (cos($h_rad + 2) + 3.8);
    $A = $N_bb * (2*$R_a + $G_a + 0.05*$B_a);
    $J_root = pow($A / $A_w, 0.5 * $c * $z);
    $J = 100 * $J_root*$J_root;                 // lightness
    $Q = (4/$c * $J_root * ($A_w + 4) * $F_L_4);  // brightness
    $t = (5e4 / 13 * $N_c * $N_cb * $e_t * sqrt($a*$a + $b*$b) / ($R_a +$G_a + 1.05 * $B_a + 0.305));
    $alpha = pow($t, 0.9) * pow(1.64 - pow(0.29, $n), 0.73);
    $C = $alpha * $J_root;                     // chroma
    $M = $C * $F_L_4;                          // colorfulness
    $s = 50 * sqrt($c * $alpha / ($A_w + 4));    // saturation
    return ['J' => $J, 'C' => $C, 'h' => $h, 'Q' => $Q, 'M' => $M, 's' => $s];
  }

  public function getHex(): string {
    return $this->hex;
  }

  public function getSrgb(): array {
    if (empty($this->srgb)) {
      $this->srgb = $this->rgbToSrgb($this->rgb);
    }
    return $this->srgb;
  }

  public function getXyz(): array {
    if (empty($this->xyz)) {
      $this->xyz = $this->srgbToXyz($this->getSrgb());
    }
    return $this->xyz;
  }

  public function getCielab(): array {
    if (empty($this->cielab)) {
      $this->cielab = $this->xyzToLab($this->getXyz());
    }
    return $this->cielab;
  }

  public function getCam16(): array {
    if (empty($this->cam16)) {
      $this->cam16 = $this->xyzToCam16($this->getXyz());
    }
    return $this->cam16;
  }

  public function getRgb(): array {
    return $this->rgb;
  }

}