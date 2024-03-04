module Helper where

import Conversions
import Prelude as P
import Data.List as L

-- Prints a list of strings to the console
printList :: [String] -> IO()
printList stringlist = P.putStr (unlines stringlist)

-- Gets the filename only from a location
getFileName :: String -> String
getFileName loc = fst (P.foldr decodeName ("", False) loc)
  where
    decodeName '.' (b, bool)  = (b, True)
    decodeName '/' (b, bool)  = (b, False)
    decodeName a (b, True)    = (a:b, True)
    decodeName a (b, False)   = (b, False)

-- Gets the location without the filename
getRelativeLocation :: String -> String
getRelativeLocation loc = case L.findIndex (L.isPrefixOf (getFileName loc)) (L.tails loc) of
  Nothing -> error "Something went wrong with determining the file location"
  Just x -> take x loc
