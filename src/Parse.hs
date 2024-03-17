module Parse where

import Helper
import Conversions
import Model
import Prelude as P
import Codec.Picture
import Codec.Picture.Types
import Graphics.Gloss
import Graphics.Gloss.Juicy

-- Main parse function
parse :: [String] -> IO NCTState
parse ["-help"] = helparg >> return EmptyState
parse ("convert":args) = convertarg args
parse string = return EmptyState

-- Help
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

-- Convert argument
convertarg :: [String] -> IO NCTState
convertarg [] = error "Could not match argument list, see -help" 

-- Manual base selection
convertarg [loc] = do 
  negative <- convertFromFile loc
  P.putStr "See UI to pick base...\n" 
    >> return (ChooseBase 0.25 (0, 0) (imageWidth negative, imageHeight negative) (fromImageRGB8 negative) (0, 0))

-- Automatic base selection
convertarg (loc:["-A"]) = do 
  negative <- convertFromFile loc
  print (findBase negative) >> P.putStr "Finds automatic base colour\n" 
    >> saveConverted (imageConvert negative (findBase negative)) loc
    >> return (Result (fromImageRGB8 (imageConvert negative (findBase negative))))

convertarg (loc:"-F":[format]) = P.putStr ("Outputs in: " P.++ format) >> return EmptyState
convertarg (loc:"-A":"-F":[format]) = P.putStr ("Finds automatic base colour and outputs in" P.++ format) >> return EmptyState
convertarg args = error "Could not match argument list, see -help\n" >> return EmptyState

-- Loads a file
convertFromFile :: String -> IO (Image PixelRGB8)
convertFromFile location = do
  dimage <- readImage location
  case dimage of
    Left err-> error "Image could not be read"
    Right dynamicImage -> return (convertRGB8 dynamicImage)

-- Saves to a file
saveConverted :: Image PixelRGB8 -> String -> IO ()
saveConverted image loc = do
  writeTiff (getRelativeLocation loc P.++ "converted_" P.++ getFileName loc P.++ ".tiff") image
