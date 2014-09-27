{-# LANGUAGE TemplateHaskell #-}
-- | functions that help you with debugging.
-- Most would make sense in the Debug.Trace module
-- There are Template Haskell versions that show you file locaiton information
module Debug.FileLocation
  (debug, debugM, debugMsg, dbg, dbgMsg, trc, ltrace, ltraceM, strace, traceId, __LOC__)
  where

import Control.Applicative ((<$>), (<*>), pure)
import Language.Haskell.TH (recConE, litE, stringL, integerL)
import Language.Haskell.TH.Syntax
import Debug.Util
import Debug.Trace (trace)
import FileLocation.LocationString (locationToString)

-- | TH  version of Debug.Trace.trace that just prints a value.
dbg :: Q Exp
dbg = do
  loc <- qLocation
  let pre = "DEBUG: " ++ (locationToString loc)
  [|(\_x -> ltrace pre _x)|]

-- | TH version  of Debug.Trace.trace that prints a value and a message
-- prefix.
dbgMsg :: String -> Q Exp
dbgMsg msg = do
  loc <- qLocation
  let pre = "DEBUG: " ++ (locationToString loc) ++ ' ' : msg
  [|(\_x -> ltrace pre _x)|]

-- | A TH version of Debug.Trace.trace that prints location information
trc :: String -> Q Exp
trc str = do
  loc <- qLocation
  let prefix = "TRACE: " ++ (locationToString loc) ++ " "
  [|trace (prefix ++ str)|]

-- | A TH monadic version of debug - print a value with location information as a stand alone expression in a monad
dbgM :: Q Exp
dbgM = do
  loc <- qLocation
  let prefix = "DEBUG: " ++ (locationToString loc) ++ " "
  [|(\_x -> ltraceM (prefix ++ show _x) _x)|]

instance Lift Loc where
    lift x = recConE 'Loc [ (,) <$> (pure 'loc_filename) <*> litE (stringL (loc_filename x))
                          , (,) <$> (pure 'loc_package) <*> litE (stringL (loc_package x))
                          , (,) <$> (pure 'loc_module) <*> litE (stringL (loc_module x))
                          , (,) <$> (pure 'loc_start) <*> [|($(litE (integerL (fromIntegral (fst (loc_start x))))),
                                                             $(litE (integerL (fromIntegral (snd (loc_start x)))))) :: (Int, Int)|]
                          , (,) <$> (pure 'loc_end) <*> [|($(litE (integerL (fromIntegral (fst (loc_end x))))),
                                                           $(litE (integerL (fromIntegral (snd (loc_end x)))))) :: (Int, Int)|] ]

-- | Embed an expression of type Loc containing the location
-- information for the place where it appears.  Could be used in
-- custom Exception types and similar:
--
-- > throw $ MyException $__LOC__
__LOC__ :: Q Exp
__LOC__ = lift =<< location
