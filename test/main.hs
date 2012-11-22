{-# LANGUAGE TemplateHaskell, DeriveDataTypeable  #-}

import Data.Data (Data, Typeable)
import FileLocation
import Control.Exception.Base (SomeException, Exception(..))
import Prelude hiding (catch)
import Control.Exception.Lifted (catch)
import Control.Monad (unless)

import System.Environment (getArgs)
import System.Process (readProcessWithExitCode)



data AException = AException String
     deriving (Show, Typeable)

instance Exception AException



main = do
    args <- getArgs
    case args of
        [] -> do
            (_, stdout, stderr) <- readProcessWithExitCode "dist/build/test/test" ["foo"] ""
            shelltest <- readFile "test/file-location.shelltest"
            let (stdout', stderr') = parseShellTest shelltest
            unless (unlines (lines stdout) == stdout') $ do
                putStrLn "Invalid stdout:"
                putStr stdout
                error "Failure"
            unless (unlines (lines stderr) == stderr') $ do
                putStrLn "Invalid stderr:"
                putStr stderr
                error "Failure"
            putStrLn "Success"
        _ -> main2


parseShellTest :: String -> (String, String)
parseShellTest orig =
    (unlines stdout, unlines stderr)
  where
    ls1 = lines orig
    ls2 = drop 1 $ dropWhile (/= ">>>") ls1
    (stdout, ls3) = break (== ">>>2") ls2
    stderr = takeWhile (/= ">>>= 1") $ drop 1 ls3

main2 :: IO ()
main2 = do
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
  $reThrow (error "foo") `catch` \e -> print ("Rethrow", e :: SomeException)
  $(err "Oh no!")
