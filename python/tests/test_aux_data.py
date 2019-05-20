import unittest
import gtirb
import IR_pb2

class AuxDataTest(unittest.TestCase):
    def test_ir_open(self):
        import os
        test_path = os.path.dirname(os.path.realpath(__file__))

        with open(os.path.join(test_path, 'test1.gtir'), 'rb') as f:
            _ir = IR_pb2.IR()
            _ir.ParseFromString(f.read()) 

            factory = gtirb.Factory()
            ir = gtirb.IR.fromProtobuf(factory, _ir)

            ad = ir._aux_data['test']
            self.assertTrue(ad is not None)
            self.assertTrue(ad._type_name == 'mapping<string,UUID>')
            self.assertTrue(sorted(list(ad._data.keys())) == ['BAR','FOO'])
            
        with open(os.path.join(test_path, 'test2.gtir'), 'rb') as f:
            _ir = IR_pb2.IR()
            _ir.ParseFromString(f.read()) 

            factory = gtirb.Factory()
            ir = gtirb.IR.fromProtobuf(factory, _ir)

            ad = ir._aux_data['test']
            ad1 = ir._aux_data['test1']

            self.assertTrue(ad._type_name == 'sequence<UUID>')
            self.assertTrue(ad1._type_name == 'sequence<string>')
            self.assertTrue(sorted(ad1._data) == ['BAR','FOO'])

        with open(os.path.join(test_path, 'test3.gtir'), 'rb') as f:
            _ir = IR_pb2.IR()
            _ir.ParseFromString(f.read()) 

            factory = gtirb.Factory()
            ir = gtirb.IR.fromProtobuf(factory, _ir)

            ad = ir._aux_data['test']
            self.assertTrue(ad is not None)
            self.assertTrue(ad._type_name == 'mapping<UUID,sequence<string>>')
            self.assertTrue(sorted(list(ad._data.values())) ==
                            [['BAZ', 'QUX'], ['FOO', 'BAR']])
        
            
if __name__ == '__main__':
    unittest.main()
