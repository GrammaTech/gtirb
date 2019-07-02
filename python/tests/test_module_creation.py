import unittest
import gtirb
import IR_pb2

class MainTest(unittest.TestCase):
    def test_module_create(self):
        factory = gtirb.Factory()
        mod = gtirb.Module(factory)
        assert mod is not None

if __name__ == '__main__':
    unittest.main()
