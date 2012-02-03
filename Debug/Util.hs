-- | Functions that help you with debugging.
-- Most would make sense in the Debug.Trace module.
module Debug.Util 
  (debug, debugM, debugMsg, debugMsgIf, ltrace, ltraceM, strace, traceId)
  where

import Debug.Trace (trace)

-- | A version of Debug.Trace.trace that just prints a value.
-- This should be included in Debug.Trace
debug :: Show a => a -> a
debug = ltrace "DEBUG"

-- | A version of Debug.Trace.trace that just prints a value and a message.
-- This should be included in Debug.Trace
debugMsg :: Show a => String -> a -> a
debugMsg msg = ltrace ("DEBUG: " ++ msg)

-- | A version of Debug.Trace.trace that just prints a value and a message.
-- This should be included in Debug.Trace
debugMsgIf :: Show a => String -> (a -> Bool) -> a -> a
debugMsgIf msg cond x = if cond x then ltrace ("DEBUG: " ++ msg) x else x

-- | Monadic debug - like debug, but works as a standalone line in a monad.
--
-- TODO: TH version with error loaction info
debugM :: (Monad m, Show a) => a -> m a
debugM a = debug a `seq` return a

-- | Trace (print on stderr at runtime) a showable expression
-- like 'debug', but do not print \"DEBUG: \".
--
-- \"strace\" stands for \"show trace\".
strace :: Show a => a -> a
strace a = trace (show a) a

-- Alias for 'strace'.
--
-- \"traceId\" means it returns itself after tracing like the 'id' function.
traceId :: Show a => a -> a
traceId = strace

-- | Labelled trace - like 'strace', but with a label prepended.
ltrace :: Show a => String -> a -> a
ltrace l a = trace (l ++ ": " ++ show a) a

-- | Monadic debug - like debug, but works as a standalone line in a monad.
--
-- TODO: TH version with error loaction info
ltraceM :: (Monad m, Show a) => String -> a -> m a
ltraceM str a = ltrace str a `seq` return a
