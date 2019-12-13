#!/bin/bash

rm -rf /gt/gtirb/build /gt/gtirb/CMakeCache.txt /gt/gtirb/CMakeFiles /gt/gtirb/CMakeScripts
mkdir -p /gt/gtirb/build
cd /gt/gtirb/build
cmake ../  -DCMAKE_CXX_COMPILER=${CXX_COMPILER}
make -j
