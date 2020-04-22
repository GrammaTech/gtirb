import sys
import tempfile

import gtirb

filename = tempfile.mktemp()
ir = gtirb.IR()
ir.save_protobuf(filename)
ir = gtirb.IR.load_protobuf(filename)
sys.exit(len(ir.modules))
