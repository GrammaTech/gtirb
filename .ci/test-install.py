import gtirb
import tempfile
import sys


filename = tempfile.mktemp()
ir = gtirb.IR()
ir.save_protobuf(filename)
ir = gtirb.IR.load_protobuf(filename)
sys.exit(len(ir.modules))
