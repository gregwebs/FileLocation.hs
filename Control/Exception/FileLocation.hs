{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Control.Exception.FileLocation
    ( thrwIO
    , thrwsIO
    , reThrow
    ) where

import Language.Haskell.TH.Syntax

import FileLocation.LocationString (locationToString)

import Control.Exception.Base hiding (throwIO)
import qualified Control.Exception.Lifted as E
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Typeable (Typeable)

throwIO :: (Exception e, MonadIO m) => e -> m a
throwIO = liftIO . E.throwIO

thrwIO :: Q Exp
thrwIO = do
  loc <- qLocation
  let locStr = locationToString loc
  [|(\_mkEx -> throwIO (_mkEx locStr))|]

thrwsIO :: String -> Q Exp
thrwsIO errMsg = do
  loc <- qLocation
  let locStr = locationToString loc
  [|(\_mkEx -> throwIO (_mkEx (locStr ++ " " ++ errMsg)))|]

data ReThrownException = ReThrownException String E.SomeException
  deriving Typeable

instance Show ReThrownException where
  show (ReThrownException s e) = "ReThrownException (" ++ s ++ "): " ++ show e

instance Exception ReThrownException

reThrow :: Q Exp
reThrow = do
  loc <- qLocation
  let locStr = locationToString loc
  [|E.handle (E.throwIO . ReThrownException locStr)|]
