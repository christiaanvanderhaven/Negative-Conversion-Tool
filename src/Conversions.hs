module Conversions where

import Prelude as P
import Codec.Picture
import Codec.Picture.Types

-- Converts all the pixels in the image
imageConvert :: Image PixelRGB8 -> PixelRGB8 -> Image PixelRGB8
imageConvert image base = pixelMap (pixelConvert base) image

imageConvertLoc :: Image PixelRGB8 -> (Int, Int) -> Image PixelRGB8
imageConvertLoc image (x, y) = imageConvert image (sampleBase image)


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


-- Tries to find the real base average from the population (the image)
sampleBase :: Image PixelRGB8 -> PixelRGB8
sampleBase image = toResult (pixelFold samplePixel ((0, 0), (0, 0), (0, 0)) image)
  where
    samplePixel :: ((Float, Float), (Float, Float), (Float, Float)) -> Int -> Int -> PixelRGB8 -> ((Float, Float), (Float, Float), (Float, Float))
    samplePixel foldData@((r_summed_total, r_total_weight), (g_summed_total, g_total_weight), (b_summed_total, b_total_weight)) x y (PixelRGB8 r_sample g_sample b_sample) = 
      if distanceToSide x y (imageWidth image) (imageWidth image) > 20 
      then foldData
      else  ( 
              newVal r_sample x y r_summed_total r_total_weight,
              newVal g_sample x y g_summed_total g_total_weight,
              newVal b_sample x y b_summed_total b_total_weight
            )
      

    newVal :: Pixel8 -> Int -> Int -> Float -> Float -> (Float, Float)
    newVal subpixel_sample x y subpixel_summed_total subpixel_total_weight = (
        subpixel_summed_total + fromIntegral subpixel_sample * distanceWeight x y (imageWidth image) (imageHeight image),--(fromIntegral subpixel_sample * normalDistribution (fromIntegral subpixel_sample) (fromIntegral subpixel_average) 50), 
        subpixel_total_weight + distanceWeight x y (imageWidth image) (imageHeight image) -- normalDistribution (fromIntegral subpixel_sample) (fromIntegral subpixel_average) 50
      )

    distanceWeightConst = 0.02

    distanceToSide :: Int -> Int -> Int -> Int -> Int
    distanceToSide x y sizex sizey = minimum (x:y:(sizex-x):[sizey-y])
    
    distanceWeight :: Int -> Int -> Int -> Int -> Float
    distanceWeight x y sizex sizey = 1.0 / (distanceWeightConst * fromIntegral (distanceToSide x y sizex sizey) + 1)


    normalDistribution :: Float -> Float -> Float -> Float
    normalDistribution x mean sd = (1/(sd * sqrt (2 * pi))) * exp ((((x - mean) / sd)  ** 2) * (-0.5))

    toResult ((r_summed_total, r_total_weight), (g_summed_total, g_total_weight), (b_summed_total, b_total_weight)) = 
      PixelRGB8
        (round (r_summed_total / r_total_weight)) 
        (round (g_summed_total / g_total_weight)) 
        (round (b_summed_total / b_total_weight))

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
  PixelRGB8 (multiply red baseRed) (multiply green baseGreen) (multiply blue baseBlue)
  where
    multiply :: Pixel8 -> Pixel8 -> Pixel8
    multiply value basevalue = floor (fromIntegral value * (255 / fromIntegral basevalue))

    highestBase :: Pixel8
    highestBase = P.maximum [baseRed, baseGreen, baseBlue]

    multiplier :: Float
    multiplier = 255 / fromIntegral highestBase
