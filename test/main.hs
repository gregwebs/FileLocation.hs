{-# LANGUAGE TemplateHaskell, DeriveDataTypeable  #-}

import Data.Data (Data, Typeable)
import FileLocation
import Control.Exception.Base (SomeException, Exception(..))
import Prelude hiding (catch)
import Control.Exception.Control (throwIO, catch)




data AException = AException String
     deriving (Show, Typeable)

instance Exception AException



main = do
  let _ = debugMsgIf "Not Visble" id False
  let x = debugMsgIf "debugMsgIf" (\xs -> head xs == 1) [1,2,3]
  putStrLn . show $ $(dbgMsg "Msg TH") $ debugMsg "Msg plain" $ $(dbg) $ debug $ $(trc "trc") x
  ltraceM "traceM" x
  debugM x
  ($thrwIO AException) `catch` \e -> putStrLn ("Caught " ++ show (e :: AException))
  ($(thrwsIO "doh!") AException) `catch` \e -> putStrLn ("Caught " ++ show (e :: AException))
  ($fromJst Nothing) `catch` \e -> putStrLn ("Caught " ++ show (e :: SomeException))
  ($fromRht (Left "Lefty")) `catch` \e -> putStrLn ("Caught " ++ show (e :: SomeException))
  $undef `catch` \e -> putStrLn ("Caught " ++ show (e :: SomeException))
  $(err "Oh no!")
