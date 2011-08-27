{-# LANGUAGE TemplateHaskell #-}
-- | see Debug.FileLocation module for more definitions
module FileLocation
  ( err, undef, fromJst, fromRht
  , debug, debugM, debugMsg, debugMsgIf, dbg, dbgMsg, trc, ltrace, ltraceM, strace
  , locationToString
  , thrwIO, thrwsIO
  )
  where

import FileLocation.LocationString (locationToString)
import Debug.FileLocation (debug, debugM, debugMsg, dbg, dbgMsg, trc, ltrace, ltraceM, strace)
import Debug.Util (debugMsgIf)
import Control.Exception.FileLocation (thrwIO, thrwsIO)
import Debug.Trace (trace)
import Language.Haskell.TH.Syntax

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

-- | like fromJust, but also shows the file location
fromJst :: Q Exp
fromJst = do
  loc <- qLocation
  let msg = (locationToString loc) ++ " fromJst: Nothing"
  [|\m -> case m of
            Just v -> v
            Nothing -> error msg|]

-- | like fromRight, but also show the file location
fromRht :: Q Exp
fromRht = do
  loc <- qLocation
  let msg = (locationToString loc) ++ " fromRht: Left: "
  [|\m -> case m of
            Right v -> v
            Left e -> error (msg ++ show e)|]
