#!/bin/bash

cd /gt/gtirb/build
cmake ../ -DCMAKE_CXX_COMPILER=${CXX_COMPILER}
make -j

TestGTIRB
