{-# LANGUAGE TemplateHaskell #-}
module Control.Exception.Control.FileLocation (thrwIO, thrwsIO) where

import Language.Haskell.TH.Syntax

import FileLocation.LocationString (locationToString)

import Control.Exception.Base hiding (throwIO)
import qualified Control.Exception as E
import Control.Monad.IO.Class (MonadIO (liftIO))

throwIO :: (Exception e, MonadIO m) => e -> m a
throwIO = liftIO . E.throwIO

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
