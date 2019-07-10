import unittest
from gtirb import Module

class MainTest(unittest.TestCase):
    def test_module_create(self):
        mod = Module()
        assert mod is not None

if __name__ == '__main__':
    unittest.main()
