#!/usr/bin/python

############################################################################
# NOTE: This example is currently out of date!
# This uses the raw Protobuf API instead of the recommended high-level API.
# See the generated Python documentation for details on the recommended API.
############################################################################

# An example program which opens an IR and prints every control-flow
# path from some basic block to another basic block.
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
#    serialized GTIRB data located at <path-to-ir>, printing every
#    control-flow path between the block with address <source-addr>
#    and the block with address <target-addr>.
#
#    $ PYTHONPATH=./python/ ./doc/examples/cfg-paths.py \
#           <path-to-ir> <source-addr> <target-addr>


from __future__ import print_function
import argparse
import collections
import sys

from IR_pb2 import IR

# Avoid duplicated effort by caching the uuids of Blocks from
# which we have know the target is unreachable.
unreachability_cache = set([])


def print_paths(blocks, edges, target_id, vertex_id, visited=set(), path=[]):
    """Depth-first search of a graph, printing all paths between two vertices.
    """

    rv = 0
    visited.add(vertex_id)
    path.append(blocks[vertex_id].address)

    # At target, print the path
    if vertex_id == target_id:
        print(", ".join("0x%.8x" % s for s in path))
        rv = 1

    # Otherwise, check all outgoing edges from this vertex
    else:
        for v in edges[vertex_id]:
            if v not in visited and v not in unreachability_cache:
                rv += print_paths(blocks, edges, target_id, v, visited, path)

    path.pop()
    # Unmark the node so it can be visited again in other paths
    visited.discard(vertex_id)
    if rv == 0:
        unreachability_cache.add(vertex_id)
    return rv


def auto_int(x):
    return int(x, 0)


parser = argparse.ArgumentParser(
    description="Print CFG paths between two blocks."
)
parser.add_argument("ir", type=argparse.FileType("rb"), help="The IR to load")
parser.add_argument(
    "source", type=auto_int, help="Address of the source block"
)
parser.add_argument(
    "target", type=auto_int, help="Address of the target block"
)
args = parser.parse_args()


def find_block(addr, mod):
    return next((b for b in mod.cfg.blocks if b.address == addr), None)


ir = IR()
ir.ParseFromString(args.ir.read())


oneblock_fmt = (
    "{0}-block {1:08X} is located in a module that does not "
    "contain  {2}-block {3:08X}.\nThe {2}-block may be in a different module, "
    "or may not exist."
)

found = False
# Check one module at a time (the GTIRB CFGs do not have edges across
# module boundaries, so if the specified blocks are in different
# modules there cannot be any paths between them).
for m in ir.modules:
    source_block = find_block(args.source, m)
    target_block = find_block(args.target, m)

    # neither block is in current module: go on to next
    if (source_block is None) and (target_block is None):
        continue

    # only one block is in current module: there cannot be any paths.
    if source_block is None:
        print(
            oneblock_fmt.format("target", args.target, "source", args.source)
        )
        sys.exit(1)
    if target_block is None:
        print(
            oneblock_fmt.format("source", args.source, "target", args.target)
        )
        sys.exit(1)

    # if execution reaches this point, both blocks are in current module
    found = True

    # Convert the CFG to a simple adjacency list
    blocks = {b.uuid: b for b in m.cfg.blocks}
    edges = collections.defaultdict(list)
    for e in m.cfg.edges:
        edges[e.source_uuid].append(e.target_uuid)

    print(
        "Paths from {0:08X} to {1:08X}".format(
            source_block.address, target_block.address
        )
    )
    num = print_paths(blocks, edges, target_block.uuid, source_block.uuid)

    print(num, "paths found.")
    break

if not found:
    print(
        "Neither block was found:\n source {0:08X}\n target {1:08X}".format(
            args.source, args.target
        )
    )
    sys.exit(1)
