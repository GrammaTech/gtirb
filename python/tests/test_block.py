import unittest

from helpers import create_interval_etc

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

    def test_block_references(self):
        ir, m, s, bi = create_interval_etc(address=0x1000, size=4)
        b1 = gtirb.CodeBlock(offset=0, size=2, byte_interval=bi)
        b2 = gtirb.CodeBlock(offset=2, size=2, byte_interval=bi)

        found = set(b1.references)
        self.assertEqual(found, set())

        s1 = gtirb.Symbol(name="hello", module=m, payload=b1)
        s2 = gtirb.Symbol(name="world", module=m, payload=b2)

        found = set(b1.references)
        self.assertEqual(found, {s1})

        found = set(b2.references)
        self.assertEqual(found, {s2})

        # Change the referent to verify we update the index
        s1.referent = b2
        found = set(b1.references)
        self.assertEqual(found, set())

        found = set(b2.references)
        self.assertEqual(found, {s1, s2})

        # Discard the symbol to verify we update the index
        m.symbols.discard(s1)
        found = set(b2.references)
        self.assertEqual(found, {s2})

        # Now add it back to verify we update the index
        m.symbols.add(s1)
        found = set(b2.references)
        self.assertEqual(found, {s1, s2})

        # Then set the payload to an integer to make sure we handle that right
        s1.value = 1
        s2.value = 1
        found = set(b2.references)
        self.assertEqual(found, set())
