# noqa: F841

import unittest

from helpers import create_interval_etc

import gtirb


class BlocksAtOffsetTests(unittest.TestCase):
    def test_blocks_at_offset_simple(self):
        ir, m, s, bi = create_interval_etc(address=None, size=4)

        # Ensure we always have a couple blocks in the index beyond what we
        # are querying so that we don't just rebuild the tree from scratch
        # every time.
        code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi)
        code_block2 = gtirb.CodeBlock(offset=1, size=1, byte_interval=bi)
        code_block3 = gtirb.CodeBlock(offset=2, size=1, byte_interval=bi)

        found = set(bi.byte_blocks_at_offset(0))
        self.assertEqual(found, {code_block})

        # Change the offset to verify we update the index
        code_block.offset = 3
        found = set(bi.byte_blocks_at_offset(0))
        self.assertEqual(found, set())

        found = set(bi.byte_blocks_at_offset(3))
        self.assertEqual(found, {code_block})

        # Discard the block to verify we update the index
        bi.blocks.discard(code_block)
        found = set(bi.byte_blocks_at_offset(3))
        self.assertEqual(found, set())

        # Now add it back to verify we update the index
        bi.blocks.add(code_block)
        found = set(bi.byte_blocks_at_offset(3))
        self.assertEqual(found, {code_block})

    def test_blocks_at_offset_overlapping(self):
        "Test that byte_blocks_at_offset only looks at starting offsets"
        ir, m, s, bi = create_interval_etc(address=None, size=4)

        code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi)
        code_block2 = gtirb.CodeBlock(offset=0, size=2, byte_interval=bi)
        code_block3 = gtirb.CodeBlock(offset=1, size=1, byte_interval=bi)

        found = set(bi.byte_blocks_at_offset(1))
        self.assertEqual(found, {code_block3})

    def test_blocks_at_offset_zero_sized(self):
        "Test that byte_blocks_at_offset can find zero-sized blocks"
        ir, m, s, bi = create_interval_etc(address=None, size=4)

        code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi)
        code_block2 = gtirb.CodeBlock(offset=1, size=0, byte_interval=bi)
        code_block3 = gtirb.CodeBlock(offset=1, size=1, byte_interval=bi)

        found = set(bi.byte_blocks_at_offset(1))
        self.assertEqual(found, {code_block2, code_block3})

    def test_blocks_at_offset_range(self):
        "Test that byte_blocks_at_offset works with ranges"
        ir, m, s, bi = create_interval_etc(address=None, size=4)

        code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi)
        code_block2 = gtirb.CodeBlock(offset=1, size=1, byte_interval=bi)
        code_block3 = gtirb.CodeBlock(offset=2, size=1, byte_interval=bi)

        found = set(bi.byte_blocks_at_offset(range(0, 1)))
        self.assertEqual(found, {code_block})

        found = set(bi.byte_blocks_at_offset(range(1, 3)))
        self.assertEqual(found, {code_block2, code_block3})

        # Now try with a range with a step to make sure that we actually
        # respect what the range tells us.
        found = set(bi.byte_blocks_at_offset(range(0, 4, 2)))
        self.assertEqual(found, {code_block, code_block3})

    def test_code_blocks_at_offset(self):
        "Test that code_blocks_at_offset only gives back CodeBlocks"
        ir, m, s, bi = create_interval_etc(address=None, size=4)

        code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi)
        data_block = gtirb.DataBlock(offset=0, size=1, byte_interval=bi)

        found = set(bi.code_blocks_at_offset(0))
        self.assertEqual(found, {code_block})

    def test_data_blocks_at_offset(self):
        "Test that data_blocks_at_offset only gives back DataBlocks"
        ir, m, s, bi = create_interval_etc(address=None, size=4)

        code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi)
        data_block = gtirb.DataBlock(offset=0, size=1, byte_interval=bi)

        found = set(bi.data_blocks_at_offset(0))
        self.assertEqual(found, {data_block})
