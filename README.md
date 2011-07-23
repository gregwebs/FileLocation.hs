  > $(err "OH NO!")
  main:Main main.hs:16:1 OH NO!

Notice how it displays package:module file:line:character
It exposes the functions err (error), undef (undefined), and trc (Debug.Trace.trace). All of these behave the same as their normal counterpart but also spit out a location.

I also included my favorite helper, debug, which is like trace but just show the value.

  > debug [1,2,3]
  DEBUG: [1,2,3]
  [1,2,3]

There is also a TH version of debug `dbg`:

  > $(dbg) [1,2,3]
  DEBUG main:Main main.hs:1:3 [1,2,3]
  [1,2,3]

Additionally the TH function `dbgMsg` can be used to add a message prefix:

  > $(dbgMsg "the list") [1,2,3]
  DEBUG main:Main main.hs:1:3 the list: [1,2,3]
  [1,2,3]