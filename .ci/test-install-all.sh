#!/bin/sh -e

set -o xtrace
set -o nounset
set -o errexit

builddir=$(pwd)
workdir=`mktemp -d`
trap 'cd / ; rm -rf $workdir' EXIT
cd $workdir

# Compile and run a C++ file that links to libgtirb
cp $builddir/.ci/test-install.cpp ./
make CXXFLAGS=-std=c++17 LDLIBS=-lgtirb test-install
./test-install

# Test that the Python package was installed
ls -l -R $workdir
python3 -m unittest discover -s $builddir/python/tests
cp $builddir/.ci/test-install.py ./
python3 test-install.py
