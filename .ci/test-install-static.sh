#!/bin/sh -e

set -o xtrace
set -o nounset
set -o errexit

builddir=$(pwd)
workdir=`mktemp -d`
trap 'cd / ; rm -rf $workdir' EXIT
cd $workdir

# Compile and run a C++ file that links to libgtirb statically
cp $builddir/.ci/test-install.cpp ./
make 'CXXFLAGS=-std=c++17' 'LDLIBS=-lgtirb -lgtirb_proto -lprotobuf -lpthread' test-install
./test-install
