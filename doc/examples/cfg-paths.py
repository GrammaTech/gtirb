#!/usr/bin/python

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
#    $ PYTHONPATH=./python/ ./doc/examples/cfg-paths.py <path-to-ir> <source-addr> <target-addr>


from __future__ import print_function
import argparse
import collections
import sys

from IR_pb2 import IR

def print_paths(blocks, edges, target_id, vertex_id, visited=set(), path=[]):
    """Depth-first search of a graph, printing all paths between two vertices."""

    visited.add(vertex_id)
    path.append(blocks[vertex_id].address)

    # At target, print the path
    if vertex_id == target_id:
        print(', '.join('0x%.8x' % s for s in path))
    else:
        # Otherwise, check all outgoing edges from this vertex
        for v in edges[vertex_id]:
            if v not in visited:
                print_paths(blocks, edges, target_id, v, visited, path)

    path.pop()
    # Unmark the node so it can be visited again in other paths
    visited.discard(vertex_id)

def auto_int(x):
    return int(x, 0)

parser = argparse.ArgumentParser(description='Print CFG paths between two blocks.')
parser.add_argument('ir', type=argparse.FileType('rb'),
                    help='The IR to load')
parser.add_argument('source', type=auto_int, help='Address of the source block')
parser.add_argument('target', type=auto_int, help='Address of the target block')
args = parser.parse_args()

ir = IR()
ir.ParseFromString(args.ir.read())

m = ir.modules[0]

def find_block(addr):
    return next((b for b in m.cfg.blocks if b.address == addr), None)
source_block = find_block(args.source)
target_block = find_block(args.target)

if source_block is None:
    print('No block at source address 0x%.8x' % args.source, file=sys.stderr)
    sys.exit(1)

if target_block is None:
    print('No block at target address 0x%.8x' % args.target, file=sys.stderr)
    sys.exit(1)

# Convert the CFG to a simple adjacency list
blocks = { b.uuid: b for b in m.cfg.blocks }
edges = collections.defaultdict(list)
for e in m.cfg.edges:
    edges[e.source_uuid].append(e.target_uuid)

print('Paths from %.8x to %.8x:' % (source_block.address, target_block.address))
print_paths(blocks, edges, target_block.uuid, source_block.uuid)
