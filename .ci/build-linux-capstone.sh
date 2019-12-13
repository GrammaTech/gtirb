#!/bin/bash

# This script is called in Dockerfile.add-capstone

# Install capstone
cd /usr/local/src
wget https://github.com/aquynh/capstone/archive/4.0.1.tar.gz
tar xf 4.0.1.tar.gz
cd capstone-4.0.1
CAPSTONE_ARCHS=x86 ./make.sh
CAPSTONE_ARCHS=x86 ./make.sh install

# Build GTIRB
cd /gt/gtirb/build
cmake ../ -DCMAKE_CXX_COMPILER=${CXX_COMPILER}
make -j

TestGTIRB
