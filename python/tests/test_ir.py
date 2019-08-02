import os
import unittest

from tempfile import NamedTemporaryFile

from gtirb import IR
from gtirb.node import Node


class IRTest(unittest.TestCase):
    def test_ir_protobuf_load(self):
        test_path = os.path.dirname(os.path.realpath(__file__))
        ir = IR.load_protobuf(os.path.join(test_path, 'test3.ir'))
        self.assertTrue(ir is not None)

    def test_ir_protobuf_save(self):
        test_path = os.path.dirname(os.path.realpath(__file__))
        ir = IR.load_protobuf(os.path.join(test_path, 'test3.ir'))
        with NamedTemporaryFile() as outfile:
            ir.save_protobuf(outfile.name)
            # Clear node cache before re-reading the IR
            Node._uuid_cache = dict()
            new_ir = IR.load_protobuf(outfile.name)
            self.assertTrue(ir.deep_eq(new_ir))


if __name__ == '__main__':
    unittest.main()
