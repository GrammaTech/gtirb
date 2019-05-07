import unittest
import gtirb
import IR_pb2
import filecmp

class TestProtobufWrite(unittest.TestCase):
    def test_ir_open(self):
        import os
        test_path = os.path.dirname(os.path.realpath(__file__))

        def open_and_compare(file_name):
            with open(os.path.join(test_path, file_name), 'rb') as f:
                _ir = IR_pb2.IR()
                _ir.ParseFromString(f.read()) 
                
                factory = gtirb.Factory()
                ir = gtirb.IR.fromProtobuf(factory, _ir)

                #print(_ir)
                
                ir_out = ir.toProtobuf()
                
                k = open('out.gtir', "wb")
                k.write(ir_out.SerializeToString())
                k.close()


                with open('out.gtir', 'rb') as h:
                    __ir = IR_pb2.IR()
                    __ir.ParseFromString(h.read())
                    
                    # Use this to compare files to see what's the difference.
                    if not _ir == __ir:
                        print("Thing we ingested(%s) not same as thing "
                              "we emitted. \n Input: \n"
                              %(os.path.join(test_path, file_name)))
                        print(_ir)
                        print("Output: \n")
                        print(__ir)
                        self.assertTrue(False)

        open_and_compare('test4.gtir')
        open_and_compare('test1.gtir')
        open_and_compare('test2.gtir')
        
if __name__ == '__main__':
    unittest.main()
