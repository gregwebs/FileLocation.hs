{-# LANGUAGE TemplateHaskell #-}
module ErrorLocation (err, undef, debug, trc) where

import Language.Haskell.TH.Syntax
import Debug.Trace

-- | like Prelude.error, but gives the file location
--
-- > $(err "OH NO!)
-- > main:Main main.hs:4:10 OH NO!
err :: String -> Q Exp
err str = do
  loc <- qLocation
  let prefix = (locationToString loc) ++ " "
  [|error (prefix ++ str)|]

-- | like Prelude.undefined, but gives the file location
-- use trace to output the location.
-- this way we still use undefined instead of calling error
--
-- > $(undef)
-- > main:Main main.hs:4:10 undefined
-- > err: Prelude.undefined
undef :: Q Exp
undef = do
  loc <- qLocation
  let prefix = (locationToString loc) ++ " "
  [|trace (prefix ++ "undefined") undefined|]

-- leaving out the loc_end parameter
locationToString :: Loc -> String
locationToString loc = (loc_package loc) ++ ":" ++ (loc_module loc) ++ " " ++
  (loc_filename loc) ++ ":" ++ (line loc) ++ ":" ++ (char loc)
  where
    line = show . fst . loc_start
    char = show . snd . loc_start

-- | A version of Debug.Trace.trace that just prints a value.
-- This should be included in Debug.Trace
debug :: Show a => a -> a
debug x = trace ("DEBUG: " ++ show x) x

-- | A TH version of Debug.Trace.trace that prints location information
-- TODO: make a debug version of this
trc :: String -> Q Exp
trc str = do
  loc <- qLocation
  let prefix = "TRACE: " ++ (locationToString loc) ++ " "
  [|trace (prefix ++ str)|]
