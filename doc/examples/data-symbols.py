#!/usr/bin/python
#
# An example program which opens an IR and prints information about all
# symbols pointing to data.
#
# To run this example, do the following.
#
# 1. Install the gtirb package from Pypi.
#
# 2. Run ddisasm to disassemble a binary to GTIRB.
#
#    $ echo 'main(){puts("hello world");}'|gcc -x c - -o /tmp/hello
#    $ ddisasm /tmp/hello --ir /tmp/hello.gtirb
#
# 3. Execute the following command to run the program on the
#    serialized GTIRB data.
#
#    $ ./doc/examples/data-symbols.py /tmp/hello.gtirb
import sys

import gtirb

if len(sys.argv) < 2:
    print(f"Usage: {sys.argv[0]} /path/to/file.gtirb")
    quit(1)

ir = gtirb.ir.IR.load_protobuf(sys.argv[1])

for m in ir.modules:
    for s in m.symbols:
        ref = s.referent
        if isinstance(ref, gtirb.block.DataBlock):
            print(s.name)
