set -e
g++ test_floats.cpp --std=c++17 -lgtirb -o test_floats
./test_floats -w floats_cpp.gtirb
python test_floats.py -w floats_py.gtirb

./test_floats -f floats_py.gtirb
python test_floats.py -f floats_cpp.gtirb
