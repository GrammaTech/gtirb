import unittest

import gtirb
from helpers import create_interval_etc


class BlocksOnOffsetTests(unittest.TestCase):
    def test_blocks_on_offset_simple(self):
        ir, m, s, bi = create_interval_etc(address=None, size=4)

        code_block = gtirb.CodeBlock(offset=0, size=3, byte_interval=bi)
        code_block2 = gtirb.CodeBlock(offset=3, size=1, byte_interval=bi)

        found = set(bi.byte_blocks_on_offset(1))
        self.assertEqual(found, {code_block})

        found = set(bi.byte_blocks_on_offset(3))
        self.assertEqual(found, {code_block2})

        # Change the offset to verify we update the index
        code_block.offset = 2
        found = set(bi.byte_blocks_on_offset(0))
        self.assertEqual(found, set())

        found = set(bi.byte_blocks_on_offset(2))
        self.assertEqual(found, {code_block})

        # Discard the block to verify we update the index
        bi.blocks.discard(code_block)
        found = set(bi.byte_blocks_on_offset(2))
        self.assertEqual(found, set())

        # Now add it back to verify we update the index
        bi.blocks.add(code_block)
        found = set(bi.byte_blocks_on_offset(2))
        self.assertEqual(found, {code_block})

    def test_blocks_on_offset_zero(self):
        ir, m, s, bi = create_interval_etc(address=None, size=4)

        code_block = gtirb.CodeBlock(offset=0, size=3, byte_interval=bi)

        found = set(bi.byte_blocks_on_offset(0))
        self.assertEqual(found, {code_block})

    def test_blocks_on_offset_with_overlapping_blocks(self):
        """
        Test that byte_blocks_on_offset returns all blocks that overlap an
        offset.
        """
        ir, m, s, bi = create_interval_etc(address=None, size=4)

        code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi)
        code_block2 = gtirb.CodeBlock(offset=0, size=2, byte_interval=bi)
        code_block3 = gtirb.CodeBlock(offset=1, size=1, byte_interval=bi)

        found = set(bi.byte_blocks_on_offset(1))
        self.assertEqual(found, {code_block2, code_block3})

    def test_blocks_on_offset_with_zero_sized_blocks(self):
        "Test that byte_blocks_on_offset doesn't find zero-sized blocks"
        ir, m, s, bi = create_interval_etc(address=None, size=4)

        code_block = gtirb.CodeBlock(offset=0, size=3, byte_interval=bi)
        code_block2 = gtirb.CodeBlock(offset=1, size=0, byte_interval=bi)
        code_block3 = gtirb.CodeBlock(offset=1, size=1, byte_interval=bi)

        found = set(bi.byte_blocks_on_offset(1))
        self.assertEqual(found, {code_block, code_block3})

        found = set(bi.byte_blocks_on_offset(range(1, 2)))
        self.assertEqual(found, {code_block, code_block3})

    def test_blocks_on_offset_with_range(self):
        "Test that byte_blocks_on_offset handles ranges"
        ir, m, s, bi = create_interval_etc(address=None, size=4)

        code_block = gtirb.CodeBlock(offset=0, size=2, byte_interval=bi)
        code_block2 = gtirb.CodeBlock(offset=2, size=1, byte_interval=bi)
        code_block3 = gtirb.CodeBlock(offset=3, size=1, byte_interval=bi)

        found = set(bi.byte_blocks_on_offset(range(0, 4)))
        self.assertEqual(found, {code_block, code_block2, code_block3})

        found = set(bi.byte_blocks_on_offset(range(2, 2)))
        self.assertEqual(found, set())

        # Passing a different step doesn't make a ton of sense, but it should
        # work.
        found = set(bi.byte_blocks_on_offset(range(0, 4, 2)))
        self.assertEqual(found, {code_block, code_block2, code_block3})

    def test_code_blocks_on_offset(self):
        "Test that code_blocks_on_offset only gives back CodeBlocks"
        ir, m, s, bi = create_interval_etc(address=None, size=4)

        code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi)
        data_block = gtirb.DataBlock(offset=1, size=1, byte_interval=bi)

        found = set(bi.code_blocks_on_offset(range(0, 4)))
        self.assertEqual(found, {code_block})

    def test_data_blocks_on_offset(self):
        "Test that data_blocks_on_offset only gives back DataBlocks"
        ir, m, s, bi = create_interval_etc(address=None, size=4)

        code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi)
        data_block = gtirb.DataBlock(offset=1, size=1, byte_interval=bi)

        found = set(bi.data_blocks_on_offset(range(0, 4)))
        self.assertEqual(found, {data_block})
