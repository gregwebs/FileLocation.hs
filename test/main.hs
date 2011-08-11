{-# LANGUAGE TemplateHaskell, DeriveDataTypeable  #-}

import Data.Data (Data, Typeable)
import FileLocation
import Control.Exception.Control (catch, try)
import Control.Exception.Base (Exception(..))
import Prelude hiding (catch)
import Control.Monad.Trans.Error (throwError, ErrorT (..), Error (..))
import Control.Monad.IO.Class
import Control.Exception.Control (throwIO)

data AException = AException String
     deriving (Show, Typeable)

instance Exception AException

runE :: MonadIO m => AException -> m ()
runE (AException str) = liftIO $ putStrLn str

main = do
  let x = [1,2,3]
  putStrLn . show $ $(dbgMsg "Msg TH") $ debugMsg "Msg plain" $ $(dbg) $ debug $ $(trc "trc") x
  ltraceM "traceM" x
  debugM x
  ($(thrwIO) $ AException) `catch` \e -> putStrLn ("Caught " ++ show (e :: AException))
  ($(thrwsIO "doh!") AException) `catch` \e -> putStrLn ("Caught " ++ show (e :: AException))
  $(undef)
  $(err "Oh no!")
