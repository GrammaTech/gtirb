import filecmp
import os
import unittest
from gtirb import IR

class TestProtobufWrite(unittest.TestCase):
    def test_protobuf_write(self):
        test_path = os.path.dirname(os.path.realpath(__file__))

        def open_and_compare(file_name):
            file_path = os.path.join(test_path, file_name)
            orig_ir = IR.load_protobuf(file_path)
            ir = IR.load_protobuf(file_path)
            ir.save_protobuf('out.gtir')
            new_ir = IR.load_protobuf('out.gtir')
            # Note: this compares files, not IRs, which means that it is
            # possible to fail this test if the contents of unordered
            # collections that we consider equivalent are serialized in
            # different orders. The fix is to implement IR comparisons.
            self.assertTrue(filecmp.cmp(file_path, 'out.gtir'))

        open_and_compare('test2.gtir')

if __name__ == '__main__':
    unittest.main()
