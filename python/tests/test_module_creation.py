import unittest
import gtirb
import IR_pb2

class MainTest(unittest.TestCase):
    def test_module_create(self):
        mod = gtirb.Module(dict())
        assert mod is not None

if __name__ == '__main__':
    unittest.main()
