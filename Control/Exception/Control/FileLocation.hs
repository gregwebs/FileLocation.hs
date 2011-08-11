{-# LANGUAGE TemplateHaskell #-}
module Control.Exception.Control.FileLocation (thrwIO, thrwsIO) where

import Language.Haskell.TH.Syntax

import FileLocation.LocationString (locationToString)

import Control.Exception.Base hiding (throwIO)
import Control.Exception.Control (throwIO)

thrwIO :: Q Exp
thrwIO = do
  loc <- qLocation
  let locStr = locationToString loc
  [|(\mkEx -> throwIO (mkEx locStr))|]

thrwsIO :: String -> Q Exp
thrwsIO errMsg = do
  loc <- qLocation
  let locStr = locationToString loc
  [|(\mkEx -> throwIO (mkEx (locStr ++ " " ++ errMsg)))|]
