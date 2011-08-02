{-# LANGUAGE CPP, TemplateHaskell #-}
import FileLocation
#include "consts.h"

main = do
  -- $(undef)
  $(err "Oh no!")
