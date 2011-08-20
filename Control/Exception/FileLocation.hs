{-# LANGUAGE TemplateHaskell #-}
module Control.Exception.FileLocation (thrwIO, thrwsIO) where

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

{- usually want to make a located vesion of a function rather than use this.
 - perhaps it could be used to quickly make a located version
-- {-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
-- import Data.Typeable (Typeable)
data ReThrownException = forall e. (Exception e, Show e) => ReThrownException String e
  deriving (Typeable)

instance Show ReThrownException where
  show (ReThrownException s e) = "ReThrownException: " ++ s ++ "\n" ++ show e

instance Exception ReThrownException

reThrow :: Q Exp
reThrow = do
  loc <- qLocation
  let locStr = locationToString loc
  [|\risky -> E.catch risky (\e -> throwIO (ReThrownException locStr e))|]
  -}
