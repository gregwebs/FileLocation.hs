Common debugging/error/exception functions and template haskell versions that give file location information

# Error/undefined

    > $(err "OH NO!")
    main:Main main.hs:16:1 OH NO!


Notice how it displays package:module file:line:character
It exposes the functions err (error), undef (undefined), and trc (Debug.Trace.trace). All of these behave the same as their normal counterpart but also spit out a location.

    > $(undef)
    main:Main test/main.hs:10:5 undefined
    main: Prelude.undefined


# Debug helpers

I also included my favorite helper, debug, which is like trace but just show the value.

    > debug [1,2,3]
    DEBUG: [1,2,3]
    [1,2,3]


There is a version without the DEBUG prefix called both strace and traceId.
And Here is the TH version of debug `dbg`:

    > $(dbg) [1,2,3]
    DEBUG main:Main main.hs:1:3 [1,2,3]
    [1,2,3]


Additionally there is a debugMsg and a TH function `dbgMsg` can be used to add a message prefix:

    > $(dbgMsg "the list") [1,2,3]
    DEBUG main:Main main.hs:1:3 the list: [1,2,3]
    [1,2,3]


Also there is debugM for stand-alone use in a monad. There is also functions ltrace and ltraceM (short for labeled trace)- it is like debug, but you tell it what string to print out instead of "DEBUG".


# Exceptions

Also there is a version of thrwIO that gives location information. thrwsIO takes appends a string to the file locaiton information.

    import Control.Exception.Base (Exception(..))
    import Prelude hiding (catch)
    import Control.Exception.Control (throwIO, catch)

    data AException = AException String deriving (Show, Typeable)
    instance Exception AException

    ($(thrwIO) AException) `catch` \e -> putStrLn ("Caught " ++ show (e :: AException))
    
    Caught AException "main:Main test/main.hs:25:6"

# Test Suite

./test/run.sh
