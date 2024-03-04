module Parse where

import Helper
import Conversions
import Prelude as P
import Codec.Picture
import Codec.Picture.Types

-- Main parse function
parse :: [String] -> IO ()
parse ["-help"] = helparg
parse ("convert":args) = convertarg args
parse string = P.putStrLn (concat string)

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
convertarg :: [String] -> IO()
convertarg [] = error "Could not match argument list, see -help"
convertarg [loc] = P.putStr "See UI to pick base...\n"
convertarg (loc:["-A"]) = do 
  negative <- convertFromFile loc
  print (findBase negative) >> P.putStr "Finds automatic base colour\n" >> saveConverted (imageConvert negative (findBase negative)) loc
convertarg (loc:"-F":[format]) = P.putStr ("Outputs in: " P.++ format P.++ "\n")
convertarg (loc:"-A":"-F":[format]) = P.putStr ("Finds automatic base colour and outputs in" P.++ format P.++ "\n")
convertarg args = error "Could not match argument list, see -help\n"

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
