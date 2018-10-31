#!/usr/bin/python

# An example program which opens an IR and prints information about all
# symbols pointing to data.
#
# Before using this, install the python protobuf library and generate
# message definitions:
#
# $ pip install protobuf
# $ mkdir -p python
# $ for f in src/proto/*.proto; do
#      protoc -Isrc/proto --python_out=python $f
#   done
#
# Then run the program like this:
# $ PYTHONPATH=./python/ ./doc/examples/data-symbols.py <path-to-ir>

from __future__ import print_function
import sys

from IR_pb2 import IR

ir = IR()
with open(sys.argv[1]) as f:
    ir.ParseFromString(f.read())

for m in ir.modules:
    print("Module %s" % (m.name or "<unnamed>"))

    data_objects = { obj.uuid: obj for _,obj in m.data.items() }
    for sym in m.symbols:
        data_ref = data_objects.get(sym.referent_uuid)
        if data_ref:
            print("%s:\t%s\t%s bytes" % (sym.name, data_ref.address, data_ref.size))
