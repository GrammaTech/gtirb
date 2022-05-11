set -e
cd build

### floating-point compatiblity test

bin/test_floats -w floats_cpp.gtirb
python3 src/test/testInterop/test_floats.py -w floats_py.gtirb

bin/test_floats -r floats_py.gtirb && \
    echo "python float AuxData work in c++"
python3 src/test/testInterop/test_floats.py -r floats_cpp.gtirb && \
    echo "c++ float AuxData work in python"
rm floats_*.gtirb

# variant compatibility test

bin/test_variants -w variants_cpp.gtirb
python3 src/test/testInterop/test_variants.py -w variants_py.gtirb
python3 src/test/testInterop/test_variants.py -r variants_cpp.gtirb && \
    echo "c++ variant AuxData work in python"
bin/test_variants -r variants_py.gtirb && \
    echo "python variant auxdata work in c++"
