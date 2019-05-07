import unittest
import gtirb
import IR_pb2

class MainTest(unittest.TestCase):
    def test_ir_open(self):
        import os
        test_path = os.path.dirname(os.path.realpath(__file__))

        with open(os.path.join(test_path, 'test4.gtir'), 'rb') as f:
            _ir = IR_pb2.IR()
            _ir.ParseFromString(f.read()) 

            factory = gtirb.Factory()
            ir = gtirb.IR.fromProtobuf(factory, _ir)
            self.assertTrue(ir is not None)

if __name__ == '__main__':
    unittest.main()
