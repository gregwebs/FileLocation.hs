{-# LANGUAGE TemplateHaskell #-}
-- | see Debug.FileLocation module for more definitions
module FileLocation
  ( err, err', undef, fromJst, fromRht, indx, indxShow
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
import Language.Haskell.TH(varE)
import Data.Maybe(fromMaybe)

-- | like Prelude.error, but gives the file location
--
-- > $(err "OH NO!")
-- > main:Main main.hs:4:10 OH NO!
err :: String -> Q Exp
err str = do
  loc <- qLocation
  let prefix = (locationToString loc) ++ " "
  [|error (prefix ++ str)|]

-- | like 'err', but the error message (to be appended to the location) is an argument of the generated expression.
--
-- > $(err) "OH NO!"
-- > main:Main main.hs:4:10 OH NO!
err' :: Q Exp
err' = do
  loc <- qLocation
  let prefix = (locationToString loc) ++ " "
  [| error . (prefix ++) |]

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
  [|\_m -> case _m of
            Just _v -> _v
            Nothing -> error msg|]

-- | like fromRight, but also show the file location
fromRht :: Q Exp
fromRht = do
  loc <- qLocation
  let msg = (locationToString loc) ++ " fromRht: Left: "
  [|\_m -> case _m of
            Right _v -> _v
            Left _e -> error (msg ++ show _e)|]

-- | like @(flip ('Data.Map.!')@, but also shows the file location in case the element isn't found.
--
-- Note: Uses the @lookup@ function from whatever @Data.Map@ module is currently in an exposed package.
indx :: Q Exp
indx = indx_common False

-- | Like 'indx', but also 'show's the looked-up element in case it isn't found.
indxShow :: Q Exp
indxShow = indx_common True

indx_common :: Bool -> Q Exp
indx_common = indxWith_common (varE (Name (mkOccName "lookup") (NameQ (mkModName "Data.Map"))))
                                -- avoid dep on containers (?)

indxWith_common :: Q Exp -> Bool -> Q Exp
indxWith_common lookupE showElt = do
  loc <- qLocation
  let msg = (locationToString loc) ++ " indx: Element not in the map"

      msgE varName = if showElt
                        then [| msg ++ ": " ++ show $(varE varName) |]
                        else [| msg |]


  [| \_x _m -> fromMaybe (error $(msgE '_x)) ($(lookupE) _x _m) |]
