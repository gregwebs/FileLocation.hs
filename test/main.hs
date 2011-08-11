{-# LANGUAGE TemplateHaskell, DeriveDataTypeable  #-}

import Data.Data (Data, Typeable)
import FileLocation
import Control.Exception.Base (Exception(..))
import Prelude hiding (catch)
import Control.Exception.Control (throwIO, catch)




data AException = AException String
     deriving (Show, Typeable)

instance Exception AException




main = do
  let x = [1,2,3]
  putStrLn . show $ $(dbgMsg "Msg TH") $ debugMsg "Msg plain" $ $(dbg) $ debug $ $(trc "trc") x
  ltraceM "traceM" x
  debugM x
  ($(thrwIO) $ AException) `catch` \e -> putStrLn ("Caught " ++ show (e :: AException))
  ($(thrwsIO "doh!") AException) `catch` \e -> putStrLn ("Caught " ++ show (e :: AException))
  $(undef)
  $(err "Oh no!")
