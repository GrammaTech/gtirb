import unittest
import gtirb
import IR_pb2
import filecmp

class TestProtobufWrite(unittest.TestCase):
    def test_ir_open(self):
        import os
        test_path = os.path.dirname(os.path.realpath(__file__))

        def open_and_compare(file_name):
            original_ir = IR_pb2.IR()
            with open(os.path.join(test_path, file_name), 'rb') as f:
                original_ir.ParseFromString(f.read())

            ir_loader = gtirb.IRLoader()
            ir = ir_loader.IRLoadFromProtobufFileName(
                os.path.join(test_path, file_name)
            )
            with open('out.gtir', 'wb') as f:
                f.write(ir.toProtobuf().SerializeToString())

            new_ir = IR_pb2.IR()
            with open('out.gtir', 'rb') as f:
                new_ir.ParseFromString(f.read())

            self.assertEqual(original_ir, new_ir)

        open_and_compare('test4.gtir')
        open_and_compare('test2.gtir')

if __name__ == '__main__':
    unittest.main()
