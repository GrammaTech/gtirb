import unittest

import gtirb


class BlockTest(unittest.TestCase):
    def test_contains_offset(self):
        node = gtirb.CodeBlock(offset=123, size=456, decode_mode=789)
        self.assertTrue(node.contains_offset(127))
        self.assertFalse(node.contains_offset(121))

    def test_contains_address(self):
        block = gtirb.CodeBlock(offset=123, size=456, decode_mode=789)
        byte_interval = gtirb.ByteInterval(  # noqa: F841
            address=0x0, size=579, blocks=(block,)
        )
        self.assertTrue(block.contains_address(323))  # addr: 0x143
        self.assertFalse(block.contains_address(107))  # addr: 0x6b
