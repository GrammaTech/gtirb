#!/bin/bash
BUILD_TYPE=$1

# Build and install capstone
pushd $HOME
mkdir /cygdrive/C/capstone-$BUILD_TYPE
wget https://github.com/aquynh/capstone/archive/4.0.1.tar.gz
tar xf 4.0.1.tar.gz
cd capstone-4.0.1
mkdir build
cd build
cmd.exe /C "C:\\VS\\VC\\Auxiliary\\Build\\vcvars64.bat && ""C:\\Program Files\\CMake\\bin\\cmake.exe"" -G ""Ninja"" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=C:\\capstone-release .."
cmd.exe /C "C:\\VS\\VC\\Auxiliary\\Build\\vcvars64.bat && ninja"
cmd.exe /C "C:\\VS\\VC\\Auxiliary\\Build\\vcvars64.bat && ninja install"
popd

./.ci/build-windows.sh $BUILD_TYPE
