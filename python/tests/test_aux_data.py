import os
import unittest

from collections import namedtuple
from uuid import UUID

from gtirb import CodeBlock, DataBlock, IR, Offset, Section, Symbol


class AuxDataTest(unittest.TestCase):
    def test_aux_data(self):
        """Test standard AuxDataTables

        The tests for the standard AuxDataTables check both the type name and
        the type of the contents after reading the tables.

        The standard AuxData Schemata are listed in AuxData.md, and these tests
        should match what is listed there.

        AuxData table entries referring to Nodes are encoded as UUIDs. Codecs
        look up these Nodes in a cache and return the actual object. It is
        legal and possible to not hit a UUID in a cache, but this should be
        detected using these tests.

        Each of the *_items_test checks that all items are of the expected type
        after reading in the aux data.

        """
        test_path = os.path.dirname(os.path.realpath(__file__))

        TableTest = namedtuple("TableTest", ["type_name", "items_test"])

        def alignment_items_test(items):
            for key, alignment in items:
                self.assertIsInstance(key, (CodeBlock, DataBlock, Section))
                self.assertIsInstance(alignment, int)

        def comments_items_test(items):
            for offset, comment in items:
                self.assertIsInstance(offset, Offset)
                self.assertIsInstance(comment, str)

        def function_items_test(items):
            for func_uuid, blocks in items:
                self.assertIsInstance(func_uuid, UUID)
                for block in blocks:
                    self.assertIsInstance(block, CodeBlock)

        def padding_items_test(items):
            for addr, padding in items:
                self.assertIsInstance(addr, int)
                self.assertIsInstance(padding, int)

        def types_items_test(items):
            for data_obj, type_name in items:
                self.assertIsInstance(data_obj, DataBlock)
                self.assertIsInstance(type_name, str)

        def symbolfwd_items_test(items):
            for sym1, sym2 in items:
                self.assertIsInstance(sym1, Symbol)
                self.assertIsInstance(sym2, Symbol)

        standard_table_tests = {
            "alignment": TableTest(
                type_name="mapping<UUID,uint64_t>",
                items_test=alignment_items_test,
            ),
            "comments": TableTest(
                type_name="mapping<Offset,string>",
                items_test=comments_items_test,
            ),
            "functionBlocks": TableTest(
                type_name="mapping<UUID,set<UUID>>",
                items_test=function_items_test,
            ),
            "functionEntries": TableTest(
                type_name="mapping<UUID,set<UUID>>",
                items_test=function_items_test,
            ),
            "symbolForwarding": TableTest(
                type_name="mapping<UUID,UUID>", items_test=symbolfwd_items_test
            ),
            "padding": TableTest(
                type_name="mapping<Addr,uint64_t>",
                items_test=padding_items_test,
            ),
            "types": TableTest(
                type_name="mapping<UUID,string>", items_test=types_items_test
            ),
        }

        def test_standard_auxdata(aux_data):
            for table, table_test in standard_table_tests.items():
                if table in aux_data:
                    with self.subTest(table):
                        self.assertEqual(
                            aux_data[table].type_name, table_test.type_name
                        )
                        table_test.items_test(aux_data[table].data.items())

        for ir_file in ("test%s.gtirb" % n for n in range(1, 3)):
            ir = IR.load_protobuf(os.path.join(test_path, ir_file))
            for module in ir.modules:
                test_standard_auxdata(module.aux_data)


if __name__ == "__main__":
    unittest.main()
