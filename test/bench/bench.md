A benchmarking attempt to compare CPP macros to template haskell functions.
A very simple macro- add file and line number information to an error function.

    cpp: FATAL ERROR: Oh no!AT: cpp.hs:7

    hs: main:Main hs.hs:7:5 Oh no!

# Running

## running the CPP file

    rm -f cpp && rm -f cpp.o && rm -f cpp.hi && time ghc --make -O2 cpp.hs

## running the hs file

    rm -f hs && rm -f hs.o && rm -f hs.hi && time ghc --make -O2 hs.hs

# Comparison

These are difficutl to compare directly to eachother. Template haskell requires dependencies that take time to link- dependencies you probably already have in your application. So instead we compare the times of calling one macro/TH function versus calling 26.

This benching was good enough for me that I didn't feel the need to actually use Criterion.

Data:

    CPP 1    CPP 30    HS 1     HS 30
    -------  -------  -------  -------
    .35-.37  .43-.46  .57-.59  .61-.64

Results

CPP line macros appear to take more time to compile than Error-Location TH functions.
It is still possible that there is more per-file invocation overead for TH than CPP (that this benchmark ignrores), even if you are already using TH in your application.
