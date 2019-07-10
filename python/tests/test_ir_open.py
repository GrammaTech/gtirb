import os
import unittest
from gtirb import IR


class MainTest(unittest.TestCase):
    def test_ir_open(self):
        test_path = os.path.dirname(os.path.realpath(__file__))
        ir = IR.load_protobuf(os.path.join(test_path, 'test4.gtir'))
        self.assertTrue(ir is not None)


if __name__ == '__main__':
    unittest.main()
