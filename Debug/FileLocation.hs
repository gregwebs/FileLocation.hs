{-# LANGUAGE TemplateHaskell #-}
module Debug.FileLocation
  (debug, debugM, debugMsg, dbg, dbgMsg, trc, ltrace, ltraceM, strace, traceId)
  where

import Language.Haskell.TH.Syntax
import Debug.Trace
import FileLocation.LocationString (locationToString)

-- | A version of Debug.Trace.trace that just prints a value.
-- This should be included in Debug.Trace
debug :: Show a => a -> a
debug = ltrace "DEBUG"

-- | A version of Debug.Trace.trace that just prints a value and a message.
-- This should be included in Debug.Trace
debugMsg :: Show a => String -> a -> a
debugMsg msg = ltrace ("DEBUG: " ++ msg)

-- | TH  version of Debug.Trace.trace that just prints a value.
dbg :: Q Exp
dbg = do
  loc <- qLocation
  let pre = "DEBUG: " ++ (locationToString loc)
  [|(\x -> ltrace pre x)|]

-- | TH version  of Debug.Trace.trace that prints a value and a message
-- prefix).
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

-- | monadic debug - like debug, but works as a standalone line in a monad
-- TODO: TH version with error loaction info
debugM :: (Monad m, Show a) => a -> m a
debugM a = debug a `seq` return a

-- | trace (print on stdout at runtime) a showable expression
-- like debug, but does not print "DEBUG: "
-- traceId is an alias for strace
-- strace stands for "show trace"
-- traceId means it returns itself after tracing like the id function
strace, traceId :: Show a => a -> a
strace a = trace (show a) a

traceId = strace

-- | labelled trace - like strace, with a label prepended
ltrace :: Show a => String -> a -> a
ltrace l a = trace (l ++ ": " ++ show a) a

-- | monadic debug - like debug, but works as a standalone line in a monad
-- TODO: TH version with error loaction info
ltraceM :: (Monad m, Show a) => String -> a -> m a
ltraceM str a = ltrace str a `seq` return a
