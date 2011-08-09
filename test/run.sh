#!/bin/bash -x
ghc --make test/main.hs && shelltest test/file-location.shelltest
