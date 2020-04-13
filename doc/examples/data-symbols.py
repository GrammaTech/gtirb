#!/usr/bin/python

############################################################################
# NOTE: This example is currently out of date!
# This uses the raw Protobuf API instead of the recommended high-level API.
# See the generated Python documentation for details on the recommended API.
############################################################################

# An example program which opens an IR and prints information about all
# symbols pointing to data.
#
# To run this example, do the following.
#
# 1. Install the protobuf compiler (protoc) from
#    https://github.com/protocolbuffers/protobuf/releases (if you have
#    not already done so).
#
# 2. Install the Python protobuf library (if you have not already done so).
#
#    $ pip install protobuf
#
# 3. Generate message definitions.
#
#    $ mkdir -p python
#    $ for f in src/proto/*.proto; do
#         protoc -Isrc/proto --python_out=python $f
#      done
#
#    This will create a number of files with names of the form
#    <bn>_pb2.py in the python/ subdirectory of your working directory
#    - one for each <bn>.proto in src/proto/ - including IR_pb2.py.
#
# 4. Execute the following command to run the program on the
#    serialized GTIRB data located at <path-to-ir>.
#
#    $ PYTHONPATH=./python/ ./doc/examples/data-symbols.py <path-to-ir>

from __future__ import print_function
import sys

from IR_pb2 import IR

ir = IR()
with open(sys.argv[1], "rb") as f:
    ir.ParseFromString(f.read())

for m in ir.modules:
    print("Module %s" % (m.name or "<unnamed>"))

    data_objects = {obj.uuid: obj for _, obj in m.data.items()}
    for sym in m.symbols:
        data_ref = data_objects.get(sym.referent_uuid)
        if data_ref:
            print(
                "%s:\t%s\t%s bytes"
                % (sym.name, data_ref.address, data_ref.size)
            )
