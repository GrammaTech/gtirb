import os
import unittest

from collections import namedtuple
from uuid import UUID

from gtirb import Addr, Block, DataObject, IR, Symbol


class AuxDataTest(unittest.TestCase):
    def test_ir_open(self):

        test_path = os.path.dirname(os.path.realpath(__file__))

        TableTest = namedtuple('TableTest', ['type_name', 'items_test'])

        def alignment_items_test(items):
            for key, alignment in items:
                # Can also be a Section in C++, but Section is not currently
                # implemented in the Python API
                self.assertIsInstance(key, (Block, DataObject))
                self.assertIsInstance(alignment, int)

        def comments_items_test(items):
            for addr, comment in items:
                self.assertIsInstance(addr, Addr)
                self.assertIsInstance(comment, str)

        def function_items_test(items):
            for func_uuid, blocks in items:
                self.assertIsInstance(func_uuid, UUID)
                for block in blocks:
                    self.assertIsInstance(block, Block)

        def padding_items_test(items):
            for addr, padding in items:
                self.assertIsInstance(addr, Addr)
                self.assertIsInstance(padding, int)

        def types_items_test(items):
            for data_obj, type_name in items:
                self.assertIsInstance(data_obj, DataObject)
                self.assertIsInstance(type_name, str)

        def symbolfwd_items_test(items):
            for sym1, sym2 in items:
                self.assertIsInstance(sym1, Symbol)
                self.assertIsInstance(sym2, Symbol)

        standard_table_tests = {
            'alignment': TableTest(type_name='mapping<UUID,uint64_t>',
                                   items_test=alignment_items_test),
            'comments': TableTest(type_name='mapping<Addr,string>',
                                  items_test=comments_items_test),
            'functionBlocks': TableTest(type_name='mapping<UUID,set<UUID>>',
                                        items_test=function_items_test),
            'functionEntries': TableTest(type_name='mapping<UUID,set<UUID>>',
                                         items_test=function_items_test),
            'symbolForwarding': TableTest(type_name='mapping<UUID,UUID>',
                                          items_test=symbolfwd_items_test),
            'padding': TableTest(type_name='mapping<Addr,uint64_t>',
                                 items_test=padding_items_test),
            'types': TableTest(type_name='mapping<UUID,string>',
                               items_test=types_items_test),
        }

        def test_standard_auxdata(aux_data):
            for table, table_test in standard_table_tests.items():
                if table in aux_data:
                    with self.subTest(table):
                        self.assertEqual(aux_data[table].type_name,
                                         table_test.type_name)
                        table_test.items_test(aux_data[table].data.items())

        for ir_file in ('test%s.ir' % n for n in range(1, 4)):
            ir = IR.load_protobuf(os.path.join(test_path, ir_file))
            for module in ir.modules:
                test_standard_auxdata(module.aux_data)


if __name__ == '__main__':
    unittest.main()
