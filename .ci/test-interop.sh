set -e
cd build
bin/test_floats -w floats_cpp.gtirb
python3 src/test/testInterop/test_floats.py -w floats_py.gtirb

bin/test_floats -r floats_py.gtirb && \
    echo "python float AuxData work in c++"
python3 src/test/testInterop/test_floats.py -r floats_cpp.gtirb && \
    echo "c++ float AuxData work in python"
rm floats_*.gtirb
