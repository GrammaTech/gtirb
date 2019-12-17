#!/bin/bash

# Called in gitlab-ci.yml

BUILD_TYPE=$1

rm -rf build CMakeCache.txt CMakeFiles CMakeScripts
mkdir build
cd build
cmd.exe /C "C:\\VS\\VC\\Auxiliary\\Build\\vcvars64.bat && C:\\PROGRA~1\\CMake\\bin\\cmake.exe -G "Ninja" -DBOOST_ROOT=\"C:\\Boost\" -DCMAKE_PREFIX_PATH=\"C:\\Program Files (x86)\\protobuf\" -DCMAKE_BUILD_TYPE=${BUILD_TYPE} .."
cmd.exe /C "C:\\VS\\VC\\Auxiliary\\Build\\vcvars64.bat && ninja"
