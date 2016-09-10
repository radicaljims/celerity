#!/bin/bash
stack build
cd .stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/workloads-exe/
./workloads-exe
