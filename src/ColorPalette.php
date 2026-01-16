<?php

namespace Esmero\Lipstick;

use InvalidArgumentException;
use RuntimeException;
use GdImage;

class ColorPalette extends \SplObjectStorage {

    public static function newFromURIorFile(string $uriOrfile) {
        $gdimage = NULL;
        $image_content = NULL;
        $isValidURL = filter_var($uriOrfile, FILTER_VALIDATE_URL) !== FALSE;
        if (!$isValidURL) {
            if (is_file($uriOrfile) && is_readable($uriOrfile)) {
                $image_content = file_get_contents($uriOrfile);
                $gdimage = imagecreatefromstring($image_content);
            }
            else {
                throw new InvalidArgumentException('Passed Image File path is invalid');
            }
        }
        else {
            $client = curl_init();
            try {
                curl_setopt($client, CURLOPT_URL, $uriOrfile);
                curl_setopt($client, CURLOPT_RETURNTRANSFER, TRUE);
                $image_content = curl_exec($client);
                if ($image_content === FALSE) {
                    throw new RuntimeException('Failed to fetch Image from URL');
                }
                else {
                    $gdimage = imagecreatefromstring($image_content);

                }
            }
            finally {
                curl_close($client);
            }
        }
        if ($gdimage instanceof GdImage) {
            return static::processImage($gdimage);
        }
    }

    public static function processImage(GdImage $image) {
        $self = new self;
        $indexed = !imageistruecolor($image);
        $w = imagesx($image);
        $h = imagesy($image);
        $pixels = $w * $h;
        if ($pixels == 0) {
            throw new RuntimeException('Loaded Image as has 0 pixels');
        }
        $colorint = 0;
        // used for % per color
        // pixelcount_per_color * 100 / $pixels;
        $onepixpercent = 100/$pixels;
        for ($x = 0; $x < $w; ++$x) {
            for ($y = 0; $y < $h; ++$y) {
                $colorint = imagecolorat($image, $x, $y);
                if ($indexed) {
                    //GIF/PNG/ETC
                    $rgba = imagecolorsforindex($image, $colorint);
                    $colorint = ($rgba['alpha'] * 16777216) +
                        ($rgba['red'] * 65536) +
                        ($rgba['green'] * 256) +
                        ($rgba['blue']);
                }
                $color_cam16_ucs = Color::newFromInt($colorint, 'D65');
                if ($self->contains($color_cam16_ucs)) {
                    $frequency = $self->offsetGet($color_cam16_ucs);
                    $frequency = $frequency + $onepixpercent;
                    $self->attach($color_cam16_ucs,$frequency);
                }
            }
        }
        return $self;
    }

}