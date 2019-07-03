import filecmp
import os
import unittest
from gtirb import IR

class TestProtobufWrite(unittest.TestCase):
    def test_ir_open(self):
        test_path = os.path.dirname(os.path.realpath(__file__))

        def open_and_compare(file_name):
            file_path = os.path.join(test_path, file_name)
            orig_ir = IR.load_protobuf(file_path)
            ir = IR.load_protobuf(file_path)
            ir.save_protobuf('out.gtir')
            new_ir = IR.load_protobuf('out.gtir')

            with open(file_path, 'rb') as orig_file, \
                 open('out.gtir', 'rb') as new_file:
                self.assertEqual(orig_file.read(), new_file.read())

        open_and_compare('test4.gtir')
        open_and_compare('test2.gtir')

if __name__ == '__main__':
    unittest.main()
