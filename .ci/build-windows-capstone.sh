#!/bin/bash
BUILD_TYPE=$1

# Install capstone
pushd $HOME
mkdir /cygdrive/C/capstone-$BUILD_TYPE
wget https://github.com/aquynh/capstone/archive/4.0.1.tar.gz
tar xf 4.0.1.tar.gz
cd capstone-4.0.1
mkdir build
cd build
cmd.exe /C "C:\\VS\\VC\\Auxiliary\\Build\\vcvars64.bat && ""C:\\Program Files\\CMake\\bin\\cmake.exe"" -G ""Ninja"" -DCMAKE_BUILD_TYPE=${BUILD_TYPE} -DCMAKE_INSTALL_PREFIX=C:\\capstone-${BUILD_TYPE} .."
cmd.exe /C "C:\\VS\\VC\\Auxiliary\\Build\\vcvars64.bat && ninja"
cmd.exe /C "C:\\VS\\VC\\Auxiliary\\Build\\vcvars64.bat && ninja install"
popd

# Build GTIRB
cd build
cmd.exe /C "C:\\VS\\VC\\Auxiliary\\Build\\vcvars64.bat && C:\\PROGRA~1\\CMake\\bin\\cmake.exe -G "Ninja" -DBOOST_ROOT=\"C:\\Boost\" -DCMAKE_PREFIX_PATH=\"C:\\Program Files (x86)\\protobuf\" -DCMAKE_BUILD_TYPE=${BUILD_TYPE} .."
cmd.exe /C "C:\\VS\\VC\\Auxiliary\\Build\\vcvars64.bat && ninja"

./bin/TestGTIRB.exe
