#!/bin/bash -x
ghc --make test/main.hs && shelltest test/error-location.shelltest
