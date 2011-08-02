import Criterion.Main

import qualified System.Process as Proc

-- NOT IN USE!

readProcess ""
 main = defaultMain [
        bgroup "fib" [ bench "10" $ 
                     , bench "35" $ whnf fib 35
                     , bench "37" $ whnf fib 37
                     ]
                    ]
