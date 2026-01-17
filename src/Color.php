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
        // We are ignoring Alpha here.
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
     * Provides Inverted Gamma Adjustment
     *
     * @param int|float $value
     *
     * @return float
     */
    private function rgbToSrgbStep(int|float $value): float {
        $value /= 255;
        return $value <= .04045 ? $value / 12.92 : pow(($value + .055) / 1.055, 2.4);
    }

    /**
     * Provides Gamma Adjustment
     *
     * @param int|float $value
     *
     * @return float
     * @see https://observablehq.com/@jrus/srgb
     */
    private function srgbToRgbStep(int|float $value): float {
        $value /= 255;
        return $value <= .0031308 ? $value * 12.92 : 1.055 * pow($value, 1/24) - 0.055;
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
     * @param array $rgb
     *
     * @return array
     */
    private function srgbToRgb(array $rgb): array {
        return [
            'R' => $this->srgbToRgbStep($rgb['R']),
            'G' => $this->srgbToRgbStep($rgb['G']),
            'B' => $this->srgbToRgbStep($rgb['B']),
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
     * @param array $xyz
     *
     * @return array{R: float, G: float, B: float}
     */
    private function xyzToSrgb(array $xyz):array {
        $mult = 100;
        // we use 0..1, here we use 0...100
        $xyz['X'] = $xyz['X'] * $mult;
        $xyz['Y'] = $xyz['Y'] * $mult;
        $xyz['Z'] = $xyz['Z'] * $mult;
        return [
            'R' => ((0.03241003232976359 * $xyz['X']) - (0.015373989694887858 * $xyz['Y']) - (0.004986158819963629 * $xyz['Z'])),
            'G' => (- 0.009692242522025166 * $xyz['X']) + (0.01875929983695176 * $xyz['Y']) + (0.00041554226340084706 * $xyz['Z']),
            'B' => ((0.0005563941985197545 * $xyz['X']) - (0.0020401120612391 * $xyz['Y']) + (0.010571489771875336 * $xyz['Z'])),
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
     * @return array{n: float|int, z: float, N_bb: float, N_cb: float, N_c: float, F_L: float, F_L_4: float|int|object, D_RGB: array|float[], A_w: float}
     */
    private function cam16Helpers():array {
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
        $adapt = function($component) use ($F_L) {
            $x = pow($F_L * abs($component) * 0.01, 0.42);
            return ($component <=> 0) * 400 * $x / ($x + 27.13);
        };
        $RGB_cw = [$RGB_w[0]*$D_RGB[0], $RGB_w[1]*$D_RGB[1], $RGB_w[2]*$D_RGB[2]];
        $RGB_aw = array_map($adapt, $RGB_cw);
        $A_w = $N_bb * (2*$RGB_aw[0] + $RGB_aw[1] + 0.05*$RGB_aw[2]);

        return [
            'n' => $n,
            'z' => $z,
            'N_bb' => $N_bb,
            'N_cb' => $N_cb,
            'N_c' => $N_c,
            'F_L' => $F_L,
            'F_L_4' => $F_L_4,
            'D_RGB' => $D_RGB,
            'A_w' => $A_w
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
        extract($this->cam16Helpers());

        $adapt = function($component) use ($F_L) {
            $x = pow($F_L * abs($component) * 0.01, 0.42);
            return ($component <=> 0) * 400 * $x / ($x + 27.13);
        };
        $func_mult = function($n1, $n2) {
            return $n1 * $n2;
        };
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
    private function cam16ToXYZ(array $jchqms):array {

        if (($jchqms['J'] == 0) || ($jchqms['Q'] == 0)) { return [0, 0, 0]; }
        extract($this->cam16Helpers());
        $D_RGB_inv = array_map(function($dc) { return  1 / $dc;}, $D_RGB);
        $h_rad = fmod($jchqms['h'], 360) *  (M_PI/180);
        $cos_h = cos($h_rad);
        $sin_h = sin($h_rad);
        $J_root = sqrt($jchqms['J'])*0.1 || 0.25 * $jchqms['c'] * $jchqms['Q'] / (($A_w + 4) * $F_L_4);
        $alpha = ($jchqms['s'] == null) ? ($jchqms['C'] || ($jchqms['M'] / $F_L_4) || 0) / $J_root
            : 0.0004*$jchqms['s']*$jchqms['s']*($A_w + 4) / $jchqms['c'];
        $t = pow($alpha * pow(1.64 - pow(0.29, $n), -0.73), 10 / 9);
        $e_t = 0.25 * (cos($h_rad + 2) + 3.8);
        $A = $A_w * pow($J_root, 2 / $jchqms['c'] / $z);
        $p_1 = 5e4 / 13 * $N_c * $N_cb * $e_t;
        $p_2 = $A / $N_bb;
        $r = 23 * ($p_2 + 0.305) * $t / (23*$p_1 + $t * (11*$cos_h + 108*$sin_h));
        $a = $r * $cos_h;
        $b = $r * $sin_h;
        $denom = 1 / 140;
        $unadapt = function($component) use ($F_L) {
            $exponent = 1/0.42;
            $constant = 100 / $F_L * pow(27.13, $exponent);
            $cabs = abs($component);
            return ($component <=> 0) * $constant * pow($cabs / (400 - $cabs), $exponent);
        };
        $RGB_c = array_map($unadapt, [(460*$p_2 + 451*$a +  288*$b) * $denom,
            (460*$p_2 - 891*$a -  261*$b) * $denom,
            (460*$p_2 - 220*$a - 6300*$b) * $denom]);
        return $this->M16_inv(MathHelper::matrix_elem_op($RGB_c, $D_RGB_inv, FALSE, $func_mult));
    }

    private function cam16ToCam16_ucs($cam16):array {
        $J = $cam16['J'];
        $M = $cam16['M'];
        $h = $cam16['h'];
        $h_rad =  fmod($h, 360) *  (M_PI/180);
        // M_E is the default, but for sakes of JS to PHP being explicit to 'e'/natural
        $M = log(1 + 0.0228 * $M, M_E) / 0.0228;
        return ['J' => 1.7 * $J / (1 + 0.007 * $J),  'a' => $M * cos($h_rad), 'b' => $M * sin($h_rad), 'M' => $M, 'h' => $h];
    }

    private function cam16ucsToCam16($cam16ucs):array {
        $J = $cam16ucs['J'];
        $M = $cam16ucs['M'];
        $h = $cam16ucs['h'];
        $h_rad =  fmod($h, 360) *  (M_PI/180);
        // M_E is the default, but for sakes of JS to PHP being explicit to 'e'/natural
        $M = log(1 + 0.0228 * $M, M_E) / 0.0228;
        return ['J' => 1.7 * $J / (1 + 0.007 * $J),  'a' => $M * cos($h_rad), 'b' => $M * sin($h_rad), 'M' => $M, 'h' => $h];
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

    public function getCam16UCS(): array {
        return $this->cam16ToCam16_ucs($this->getCam16());
    }

    public function getRgb(): array {
        return $this->rgb;
    }

}