#!/bin/sh -e

workdir=`mktemp -d`
trap 'cd / ; rm -rf $workdir' EXIT
cd $workdir

# Compile and run a C++ file that links to libgtirb
cp /gt/gtirb/.ci/test-install.cpp ./
make CXXFLAGS=-std=c++17 LDLIBS=-lgtirb test-install
./test-install

# Test that the Python package was installed
python3 -m unittest discover -s /gt/gtirb/python/tests
cp /gt/gtirb/.ci/test-install.py ./
python3 test-install.py
