import os
import unittest

from tempfile import NamedTemporaryFile

from gtirb import IR


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
            self.assertEqual(ir, IR.load_protobuf(outfile.name))


if __name__ == '__main__':
    unittest.main()
