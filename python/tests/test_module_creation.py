import unittest
from gtirb.module import Module


class MainTest(unittest.TestCase):
    def test_module_create(self):
        mod = Module(uuid_cache={})
        assert mod is not None


if __name__ == '__main__':
    unittest.main()
