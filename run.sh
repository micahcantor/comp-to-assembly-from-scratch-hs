#!/bin/bash

cabal build
cabal exec comp-to-assembly-from-scratch-hs -- $1 $2