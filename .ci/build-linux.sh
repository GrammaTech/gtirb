#!/bin/bash

# This script is called in the linux Dockerfiles.

# Common-Lisp specific setup.
sbcl --load /usr/share/cl-quicklisp/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(let ((ql-util::*do-not-prompt* t)) (ql:add-to-init-file))' \
    --eval '(ql:quickload :protobuf)' \
    --eval '(sb-ext:exit)'
git clone https://github.com/brown/protobuf /root/quicklisp/local-projects/protobuf
cd /root/quicklisp/local-projects/protobuf/protoc/lisp && INSTALL_ROOT=/usr/ PROTOC_ROOT=/usr/ make install
git clone https://github.com/eschulte/simpler-documentation-template /root/quicklisp/local-projects/simpler-documentation-template

# Ensure all Common Lisp dependencies are installed before the build.
# Works around the fact that you can't run multiple Quicklisp dependency installs in parallel as done by 'make -j'.
cd /gt/gtirb/build
sbcl --load /usr/share/cl-quicklisp/quicklisp.lisp \
    --eval '(asdf:initialize-source-registry `(:source-registry (:tree "/gt/gtirb/cl") :inherit-configuration))' \
    --eval '(ql:quickload :gtirb/test)' \
    --eval '(sb-ext:exit)'

# Build GTIRB.
rm -rf /gt/gtirb/build /gt/gtirb/CMakeCache.txt /gt/gtirb/CMakeFiles /gt/gtirb/CMakeScripts
mkdir -p /gt/gtirb/build
cmake ../  -DCMAKE_CXX_COMPILER=${CXX_COMPILER}
make -j
