{-# LANGUAGE TemplateHaskell #-}
-- | functions that help you with debugging.
-- Most would make sense in the Debug.Trace module
-- There are Template Haskell versions that show you file locaiton information
module Debug.FileLocation
  (debug, debugM, debugMsg, dbg, dbgMsg, trc, ltrace, ltraceM, strace, traceId)
  where

import Language.Haskell.TH.Syntax
import Debug.Util
import Debug.Trace (trace)
import FileLocation.LocationString (locationToString)

-- | TH  version of Debug.Trace.trace that just prints a value.
dbg :: Q Exp
dbg = do
  loc <- qLocation
  let pre = "DEBUG: " ++ (locationToString loc)
  [|(\x -> ltrace pre x)|]

-- | TH version  of Debug.Trace.trace that prints a value and a message
-- prefix.
dbgMsg :: String -> Q Exp
dbgMsg msg = do
  loc <- qLocation
  let pre = "DEBUG: " ++ (locationToString loc) ++ ' ' : msg
  [|(\x -> ltrace pre x)|]

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
  [|(\x -> ltraceM (prefix ++ show x) x)|]
