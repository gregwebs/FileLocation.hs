{-# LANGUAGE TemplateHaskell #-}
import ErrorLocation

main = let x = [1,2,3]
       in putStrLn . show $ $(trc "WTF?") x
