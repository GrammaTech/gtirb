import unittest
import gtirb
import IR_pb2

class MainTest(unittest.TestCase):
    def test_ir_open(self):
        import os
        test_path = os.path.dirname(os.path.realpath(__file__))

        with open(os.path.join(test_path, 'test4.gtir'), 'rb') as f:
            ir = IR_pb2.IR()
            ir.ParseFromString(f.read())

            factory = gtirb.Factory()
            ir = gtirb.IR.fromProtobuf(factory, ir)
            self.assertTrue(ir is not None)

if __name__ == '__main__':
    unittest.main()
