module FileLocation.Internal (locationToString) where

import Language.Haskell.TH.Syntax

-- leaving out the loc_end parameter
locationToString :: Loc -> String
locationToString loc = (loc_package loc) ++ ':' : (loc_module loc) ++ 
  ' ' : (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
  where
    line = show . fst . loc_start
    char = show . snd . loc_start
