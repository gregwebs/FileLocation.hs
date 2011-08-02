{-# LANGUAGE TemplateHaskell #-}
import FileLocation

main = do
  let x = [1,2,3]
  putStrLn . show $ $(dbgMsg "Msg TH") $ debugMsg "Msg plain" $ $(dbg) $ debug $ $(trc "trc") x
  ltraceM "traceM" x
  debugM x
  $(undef)
  $(err "Oh no!")
