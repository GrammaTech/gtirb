#!/bin/bash
set -e
cd build

failures=0

check() {
    local creator=$1
    local auxdata=$2
    local consumer=$3
    shift 3

    if "$@" ; then
        echo $creator $auxdata AuxData work in $consumer
    else
        failures=$(( failures + 1))
    fi
}

### floating-point compatiblity test

bin/test_floats -w floats_cpp.gtirb
python3 src/test/testInterop/test_floats.py -w floats_py.gtirb

check python float c++ \
    bin/test_floats -r floats_py.gtirb
check c++ float python \
    python3 src/test/testInterop/test_floats.py -r floats_cpp.gtirb

rm floats_{cpp,py}.gtirb

### variant compatibility test

bin/test_variants -w variants_cpp.gtirb
python3 src/test/testInterop/test_variants.py -w variants_py.gtirb

check c++ variant python \
    python3 src/test/testInterop/test_variants.py -r variants_cpp.gtirb
check python variant c++ \
    bin/test_variants -r variants_py.gtirb

rm variants_{cpp,py}.gtirb

test $failures = 0
