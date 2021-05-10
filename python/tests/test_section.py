import unittest

import gtirb


class SectionTests(unittest.TestCase):
    def test_byte_blocks(self):
        s = gtirb.Section()

        bi1 = gtirb.ByteInterval(size=4, section=s)
        bi1_code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi1)
        bi1_data_block = gtirb.DataBlock(offset=1, size=1, byte_interval=bi1)

        bi2 = gtirb.ByteInterval(size=4, section=s)
        bi2_code_block = gtirb.CodeBlock(offset=0, size=1, byte_interval=bi2)
        bi2_data_block = gtirb.DataBlock(offset=1, size=1, byte_interval=bi2)

        self.assertEqual(
            set(s.byte_blocks),
            {bi1_code_block, bi1_data_block, bi2_code_block, bi2_data_block},
        )
        self.assertEqual(set(s.code_blocks), {bi1_code_block, bi2_code_block})
        self.assertEqual(set(s.data_blocks), {bi1_data_block, bi2_data_block})
