import os
import unittest
from gtirb.ir import IR


class AuxDataTest(unittest.TestCase):
    def test_ir_open(self):

        test_path = os.path.dirname(os.path.realpath(__file__))

        ir = IR.load_protobuf(os.path.join(test_path, 'test1.gtir'))
        ad = ir.aux_data['test']
        self.assertTrue(ad is not None)
        self.assertTrue(ad.type_name == 'mapping<string,UUID>')
        self.assertTrue(sorted(list(ad.data.keys())) == ['BAR', 'FOO'])

        ir = IR.load_protobuf(os.path.join(test_path, 'test2.gtir'))
        ad = ir.aux_data['test']
        ad1 = ir.aux_data['test1']
        self.assertTrue(ad.type_name == 'sequence<UUID>')
        self.assertTrue(ad1.type_name == 'sequence<string>')
        self.assertTrue(sorted(ad1.data) == ['BAR', 'FOO'])

        ir = IR.load_protobuf(os.path.join(test_path, 'test3.gtir'))
        ad = ir.aux_data['test']
        self.assertTrue(ad is not None)
        self.assertTrue(ad.type_name == 'mapping<UUID,sequence<string>>')
        self.assertTrue(
            sorted(list(ad.data.values())) == [['BAZ', 'QUX'], ['FOO', 'BAR']]
        )


if __name__ == '__main__':
    unittest.main()
