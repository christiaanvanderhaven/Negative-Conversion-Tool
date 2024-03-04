module Main where

import Prelude as P
import Data.Vector.Storable as V
import System.Environment
import System.Exit
import Data.Bits
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Codec.Picture
import Codec.Picture.Types
import Data.ByteString.Lazy
import Graphics.Gloss.Juicy

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

main :: IO ()
main = do 
      args <- getArgs
      parse args
      --saveConverted positive
      --display window background (fromImageRGB8 positive)

parse :: [String] -> IO ()
parse ["-help"] = helparg
parse ("convert":args) = convertarg args
parse string = P.putStrLn (P.foldr (P.++) "" string)

printList :: [String] -> IO()
printList stringlist = P.putStr (P.foldr ((P.++) . (P.++ "\n")) "" stringlist)

getFileName :: String -> String
getFileName loc = fst (P.foldr decodeName ("", False) loc)
  where
    decodeName '.' (b, bool) = (b, True)
    decodeName '/' (b, bool) = (b, False)
    decodeName a (b, True) = (a:b, True)
    decodeName a (b, False) = (b, False)

helparg :: IO ()
helparg = printList [
                    "[ ALL COMMANDS ]",
                    "Supported file formats:",
                    "",
                    "",
                    "[ CONVERSION ]",
                    "convert directory [-A] [-F format]",
                    "",
                    "FLAGS:",
                    "-A            Automatic conversion without UI",
                    "-F format     Sets the output format"
                  ]

convertarg :: [String] -> IO()
convertarg [] = error "Could not match argument list, see -help"
convertarg [loc] = P.putStr "See UI to pick base...\n"
convertarg (loc:["-A"]) = do 
  negative <- convertFromFile loc
  P.putStrLn (show (findBase negative)) >> P.putStr "Finds automatic base colour\n" >> saveConverted (imageConvert negative (findBase negative)) loc
convertarg (loc:"-F":[format]) = P.putStr ("Outputs in: " P.++ format P.++ "\n")
convertarg (loc:"-A":"-F":[format]) = P.putStr ("Finds automatic base colour and outputs in" P.++ format P.++ "\n")
convertarg args = error "Could not match argument list, see -help\n"

convertFromFile :: String -> IO (Image PixelRGB8)
convertFromFile location = do
  dimage <- readImage location
  case dimage of
    Left err-> error "Image could not be read"
    Right dynamicImage -> return (convertRGB8 dynamicImage)

saveConverted :: Image PixelRGB8 -> String -> IO ()
saveConverted image loc = do
  writeTiff ("converted_" P.++ (getFileName loc) P.++ ".tiff") image

imageConvert :: Image PixelRGB8 -> PixelRGB8 -> Image PixelRGB8
imageConvert image base = pixelMap (pixelConvert base) image

findBase :: Image PixelRGB8 -> PixelRGB8
findBase image = (pixelFold findBrightest (PixelRGB8 0 0 0) image)
  where
    findBrightest a _ _ b = 
      if brightness a > brightness b
      then a
      else b
    
    brightness (PixelRGB8 red green blue) = fromIntegral red + fromIntegral green + fromIntegral blue

pixelConvert :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
pixelConvert base pixel = correctGamma 0.7  (correctWhitePoint (removeBase base pixel) base)

removeBase :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
removeBase (PixelRGB8 red green blue) (PixelRGB8 baseRed baseGreen baseBlue) = 
  PixelRGB8 (subtract red baseRed) (subtract green baseGreen) (subtract blue baseBlue)
  where
    subtract :: Pixel8 -> Pixel8 -> Pixel8
    subtract pixel base = 
      if pixel > base 
      then pixel - base
      else 0

correctGamma :: Float -> PixelRGB8 -> PixelRGB8
correctGamma y (PixelRGB8 r g b) = PixelRGB8 (correctValue r) (correctValue g) (correctValue b)
  where
    correctValue :: Pixel8 -> Pixel8
    correctValue val = floor  (255.0 * ((fromIntegral val / 255.0) ** y))

-- Choose highest basecolour value and multiply to achieve better white point
correctWhitePoint :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
correctWhitePoint (PixelRGB8 red green blue) base@(PixelRGB8 baseRed baseGreen baseBlue) = 
  PixelRGB8 (multiply red) (multiply green) (multiply blue)
  where
    multiply :: Pixel8 -> Pixel8
    multiply value = floor ((fromIntegral value) * multiplier)

    highestBase :: Pixel8
    highestBase = P.maximum [baseRed, baseGreen, baseBlue]

    multiplier :: Float
    multiplier = (fromIntegral 255) / (fromIntegral (highestBase))
