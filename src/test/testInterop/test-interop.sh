set -e
@test_floats@ -w floats_cpp.gtirb
python3 test_floats.py -w floats_py.gtirb

@test_floats@ -r floats_py.gtirb
python3 test_floats.py -r floats_cpp.gtirb
rm floats_*.gtirb
