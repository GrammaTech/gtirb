#!/usr/bin/python
#
# An example program which opens an IR and prints all paths between
# two blocks.
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
#    $ ./doc/examples/cfg-paths.py /tmp/hello.gtirb
import sys

import gtirb
import networkx as nx

if len(sys.argv) < 4:
    print(f"Usage: {sys.argv[0]} /path/to/file.gtirb source target")
    quit(1)

ir = gtirb.ir.IR.load_protobuf(sys.argv[1])
G = nx.DiGraph()

for edge in ir.cfg:
    if isinstance(edge.target, gtirb.block.ProxyBlock):
        # Represent ProxyBlocks (which don't have an address) with their UUID.
        G.add_edge(edge.source.address, edge.target.uuid)
    else:
        G.add_edge(edge.source.address, edge.target.address)

for path in nx.all_simple_paths(G, int(sys.argv[2]), int(sys.argv[3])):
    print(path)
