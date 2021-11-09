# noqa: F841

import unittest

import gtirb
from helpers import SearchScope, create_interval_etc, parameterize_one


class BlocksAtTests(unittest.TestCase):
    @parameterize_one("scope", list(SearchScope))
    def test_blocks_at_simple(self, scope):
        ir, m, s, bi = create_interval_etc(address=0x1000, size=4)
        search_in = scope.select(ir, m, s, bi)

        code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi)
        code_block2 = gtirb.CodeBlock(offset=1, size=1, byte_interval=bi)

        found = set(search_in.byte_blocks_at(0x1000))
        self.assertEqual(found, {code_block})

        # Change the offset to verify we update the index
        code_block.offset = 2
        found = set(search_in.byte_blocks_at(0x1000))
        self.assertEqual(found, set())

        found = set(search_in.byte_blocks_at(0x1002))
        self.assertEqual(found, {code_block})

        # Discard the block to verify we update the index
        bi.blocks.discard(code_block)
        found = set(search_in.byte_blocks_at(0x1002))
        self.assertEqual(found, set())

        # Now add it back to verify we update the index
        bi.blocks.add(code_block)
        found = set(search_in.byte_blocks_at(0x1002))
        self.assertEqual(found, {code_block})

    @parameterize_one("scope", list(SearchScope))
    def test_blocks_at_zero(self, scope):
        ir, m, s, bi = create_interval_etc(address=0, size=4)
        search_in = scope.select(ir, m, s, bi)

        code_block = gtirb.CodeBlock(offset=0, size=3, byte_interval=bi)

        found = set(search_in.byte_blocks_at(0))
        self.assertEqual(found, {code_block})

    @parameterize_one("scope", list(SearchScope))
    def test_blocks_at_overlapping(self, scope):
        "Test that byte_blocks_at only looks at starting addresses"
        ir, m, s, bi = create_interval_etc(address=0x1000, size=4)
        search_in = scope.select(ir, m, s, bi)

        code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi)
        code_block2 = gtirb.CodeBlock(offset=0, size=2, byte_interval=bi)
        code_block3 = gtirb.CodeBlock(offset=1, size=1, byte_interval=bi)

        found = set(search_in.byte_blocks_at(0x1001))
        self.assertEqual(found, {code_block3})

    @parameterize_one("scope", list(SearchScope))
    def test_blocks_at_zero_sized(self, scope):
        "Test that byte_blocks_at can find zero-sized blocks"
        ir, m, s, bi = create_interval_etc(address=0x1000, size=4)
        search_in = scope.select(ir, m, s, bi)

        code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi)
        code_block2 = gtirb.CodeBlock(offset=1, size=0, byte_interval=bi)
        code_block3 = gtirb.CodeBlock(offset=1, size=1, byte_interval=bi)
        search_in = scope.select(ir, m, s, bi)

        found = set(search_in.byte_blocks_at(0x1001))
        self.assertEqual(found, {code_block2, code_block3})

    @parameterize_one("scope", list(SearchScope))
    def test_blocks_at_range(self, scope):
        "Test that byte_blocks_at works with ranges"
        ir, m, s, bi = create_interval_etc(address=0x1000, size=4)
        search_in = scope.select(ir, m, s, bi)

        code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi)
        code_block2 = gtirb.CodeBlock(offset=1, size=1, byte_interval=bi)
        code_block3 = gtirb.CodeBlock(offset=2, size=1, byte_interval=bi)

        found = set(search_in.byte_blocks_at(range(0x1000, 0x1001)))
        self.assertEqual(found, {code_block})

        found = set(search_in.byte_blocks_at(range(0x1001, 0x1003)))
        self.assertEqual(found, {code_block2, code_block3})

        # Now try with a range with a step to make sure that we actually
        # respect what the range tells us.
        found = set(search_in.byte_blocks_at(range(0x1000, 0x1004, 2)))
        self.assertEqual(found, {code_block, code_block3})

    @parameterize_one("scope", list(SearchScope))
    def test_blocks_at_no_address(self, scope):
        "Test that byte_blocks_at does nothing if we don't have an address"
        ir, m, s, bi = create_interval_etc(address=None, size=4)
        search_in = scope.select(ir, m, s, bi)

        code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi)
        code_block2 = gtirb.CodeBlock(offset=1, size=1, byte_interval=bi)

        found = set(search_in.byte_blocks_at(0x1000))
        self.assertEqual(found, set())

    @parameterize_one("scope", list(SearchScope))
    def test_code_blocks_at(self, scope):
        "Test that code_blocks_at only gives back CodeBlocks"
        ir, m, s, bi = create_interval_etc(address=0x1000, size=4)
        search_in = scope.select(ir, m, s, bi)

        code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi)
        data_block = gtirb.DataBlock(offset=0, size=1, byte_interval=bi)

        found = set(search_in.code_blocks_at(0x1000))
        self.assertEqual(found, {code_block})

    @parameterize_one("scope", list(SearchScope))
    def test_data_blocks_at(self, scope):
        "Test that data_blocks_at only gives back DataBlocks"
        ir, m, s, bi = create_interval_etc(address=0x1000, size=4)
        search_in = scope.select(ir, m, s, bi)

        code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi)
        data_block = gtirb.DataBlock(offset=0, size=1, byte_interval=bi)

        found = set(search_in.data_blocks_at(0x1000))
        self.assertEqual(found, {data_block})


class SectionBlocksAtTests(unittest.TestCase):
    def test_blocks_at_simple(self):
        s = gtirb.Section()

        bi1 = gtirb.ByteInterval(address=0x1000, size=4, section=s)
        bi1_block1 = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi1)
        bi1_block2 = gtirb.CodeBlock(offset=1, size=1, byte_interval=bi1)

        bi2 = gtirb.ByteInterval(address=0x1004, size=4, section=s)
        bi2_block1 = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi2)
        bi2_block2 = gtirb.CodeBlock(offset=1, size=1, byte_interval=bi2)

        found = set(s.byte_blocks_at(0x1000))
        self.assertEqual(found, {bi1_block1})

    def test_blocks_at_overlapping(self):
        "Test that we find the correct blocks if two byte intervals overlap"
        s = gtirb.Section()

        bi1 = gtirb.ByteInterval(address=0x1000, size=4, section=s)
        bi1_block1 = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi1)
        bi1_block2 = gtirb.CodeBlock(offset=1, size=1, byte_interval=bi1)

        bi2 = gtirb.ByteInterval(address=0x1000, size=4, section=s)
        bi2_block1 = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi2)
        bi2_block2 = gtirb.CodeBlock(offset=1, size=1, byte_interval=bi2)

        found = set(s.byte_blocks_at(0x1000))
        self.assertEqual(found, {bi1_block1, bi2_block1})

    def test_blocks_on_with_blocks_outside_bi(self):
        "Tests that we can handle byte intervals with blocks outside"
        s = gtirb.Section()

        bi1 = gtirb.ByteInterval(address=0x1000, size=1, section=s)
        bi1_block1 = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi1)
        bi1_block2 = gtirb.CodeBlock(offset=1, size=1, byte_interval=bi1)

        found = set(s.byte_blocks_at(0x1000))
        self.assertEqual(found, {bi1_block1})

        # These blocks are outside of the byte interval's declared size, so
        # either interpretation is fair game.
        found = set(s.byte_blocks_at(0x1001))
        self.assertTrue(found == set() or found == {bi1_block2})
