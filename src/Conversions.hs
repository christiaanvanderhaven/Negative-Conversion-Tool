module Conversions where

import Prelude as P
import Codec.Picture
import Codec.Picture.Types

-- Converts all the pixels in the image
imageConvert :: Image PixelRGB8 -> PixelRGB8 -> Image PixelRGB8
imageConvert image base = pixelMap (pixelConvert base) image

-- Finds the base of the negative
-- Currently achieved through finding the brightest pixel in the image
findBase :: Image PixelRGB8 -> PixelRGB8
findBase image = pixelFold findBrightest (PixelRGB8 0 0 0) image
  where
    findBrightest a _ _ b = 
      if brightness a > brightness b
      then a
      else b
    
    brightness (PixelRGB8 red green blue) = fromIntegral red + fromIntegral green + fromIntegral blue

-- Converts a single pixel
pixelConvert :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
pixelConvert base pixel = correctGamma 0.6 (correctWhitePoint (removeBase base pixel) base)

-- Eliminates the given base
removeBase :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
removeBase (PixelRGB8 red green blue) (PixelRGB8 baseRed baseGreen baseBlue) = 
  PixelRGB8 (subtract red baseRed) (subtract green baseGreen) (subtract blue baseBlue)
  where
    subtract :: Pixel8 -> Pixel8 -> Pixel8
    subtract pixel base = 
      if pixel > base 
      then pixel - base
      else 0

-- Applies gamma function to better divide tones across RGB space
correctGamma :: Float -> PixelRGB8 -> PixelRGB8
correctGamma y (PixelRGB8 r g b) = PixelRGB8 (correctValue r) (correctValue g) (correctValue b)
  where
    correctValue :: Pixel8 -> Pixel8
    correctValue val = floor  (255.0 * ((fromIntegral val / 255.0) ** y))

-- Choose highest basecolour value and multiply to achieve better white point
-- 'Stretches' the image to fill the full RGB space
correctWhitePoint :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
correctWhitePoint (PixelRGB8 red green blue) base@(PixelRGB8 baseRed baseGreen baseBlue) = 
  PixelRGB8 (multiply red) (multiply green) (multiply blue)
  where
    multiply :: Pixel8 -> Pixel8
    multiply value = floor (fromIntegral value * multiplier)

    highestBase :: Pixel8
    highestBase = P.maximum [baseRed, baseGreen, baseBlue]

    multiplier :: Float
    multiplier = 255 / fromIntegral highestBase
