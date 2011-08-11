{-# LANGUAGE TemplateHaskell #-}
-- | see Debug.FileLocation module for more definitions
module FileLocation
  ( err, undef
  , debug, debugM, debugMsg, dbg, dbgMsg, trc, ltrace, ltraceM, strace
  , locationToString
  , thrwIO, thrwsIO
  )
  where

import FileLocation.LocationString (locationToString)
import Debug.FileLocation (debug, debugM, debugMsg, dbg, dbgMsg, trc, ltrace, ltraceM, strace)
import Control.Exception.Control.FileLocation (thrwIO, thrwsIO)
import Debug.Trace (trace)
-- future plans
-- import Control.Exception.FileLocation (thrw, thrwIO)

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
