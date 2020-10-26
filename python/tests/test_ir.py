import os
import tempfile
import unittest

import gtirb

IR_FILE = tempfile.mktemp(suffix=".gtirb")


class IRTest(unittest.TestCase):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        ir = gtirb.IR()
        m = gtirb.Module(
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.RAW,
            isa=gtirb.Module.ISA.ValidButUnsupported,
            name="name",
            preferred_addr=1,
            rebase_delta=2,
        )
        m.ir = ir
        s = gtirb.Section(
            name="name",
            flags=(
                gtirb.Section.Flag.Executable,
                gtirb.Section.Flag.Readable,
                gtirb.Section.Flag.Loaded,
                gtirb.Section.Flag.Initialized,
            ),
        )
        s.module = m
        bi = gtirb.ByteInterval(address=0, size=10, contents=b"abcd")
        bi.section = s
        cb = gtirb.CodeBlock(size=4, offset=0, decode_mode=1)
        cb.byte_interval = bi
        db = gtirb.DataBlock(size=6, offset=4)
        db.byte_interval = bi
        sym = gtirb.Symbol(name="name", payload=cb)
        sym.module = m
        sac = gtirb.SymAddrConst(0, sym, {gtirb.SymAttribute.Part1})
        bi.symbolic_expressions[2] = sac
        p = gtirb.ProxyBlock()
        p.module = m
        ir.cfg.add(
            gtirb.Edge(
                cb,
                p,
                gtirb.Edge.Label(
                    gtirb.Edge.Type.Branch, conditional=False, direct=True
                ),
            )
        )
        m.aux_data["key"] = gtirb.AuxData(gtirb.Offset(s, 777), "Offset")
        ir.aux_data["key"] = gtirb.AuxData("value", "string")

        self.ir = ir

    def setUp(self):
        self.ir.save_protobuf(IR_FILE)

    def tearDown(self):
        os.remove(IR_FILE)

    def test_ir_protobuf_load(self):
        new_ir = gtirb.IR.load_protobuf(IR_FILE)
        self.assertTrue(self.ir.deep_eq(new_ir))
        self.assertNotEqual(
            self.ir.modules[0].aux_data["key"].data,
            new_ir.modules[0].aux_data["key"].data,
        )


if __name__ == "__main__":
    unittest.main()
