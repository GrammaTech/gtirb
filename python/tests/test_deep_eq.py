import unittest
import uuid

import gtirb


class DeepEqTest(unittest.TestCase):
    def test_code_block(self):
        id1 = uuid.uuid4()
        id2 = uuid.uuid4()

        b1 = gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id1)
        b2 = gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id1)
        self.assertTrue(b1.deep_eq(b2))

        b2 = gtirb.CodeBlock(size=5, decode_mode=2, offset=3, uuid=id1)
        self.assertFalse(b1.deep_eq(b2))

        b2 = gtirb.CodeBlock(size=1, decode_mode=5, offset=3, uuid=id1)
        self.assertFalse(b1.deep_eq(b2))

        b2 = gtirb.CodeBlock(size=1, decode_mode=2, offset=5, uuid=id1)
        self.assertFalse(b1.deep_eq(b2))

        b2 = gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id2)
        self.assertFalse(b1.deep_eq(b2))

    def test_data_block(self):
        id1 = uuid.uuid4()
        id2 = uuid.uuid4()

        b1 = gtirb.DataBlock(size=1, offset=3, uuid=id1)
        b2 = gtirb.DataBlock(size=1, offset=3, uuid=id1)
        self.assertTrue(b1.deep_eq(b2))

        b2 = gtirb.DataBlock(size=5, offset=3, uuid=id1)
        self.assertFalse(b1.deep_eq(b2))

        b2 = gtirb.DataBlock(size=1, offset=5, uuid=id1)
        self.assertFalse(b1.deep_eq(b2))

        b2 = gtirb.DataBlock(size=1, offset=3, uuid=id2)
        self.assertFalse(b1.deep_eq(b2))

    def test_proxy_blocks(self):
        id1 = uuid.uuid4()
        id2 = uuid.uuid4()

        b1 = gtirb.ProxyBlock(uuid=id1)
        b2 = gtirb.ProxyBlock(uuid=id1)
        self.assertTrue(b1.deep_eq(b2))

        b2 = gtirb.ProxyBlock(uuid=id2)
        self.assertFalse(b1.deep_eq(b2))

    def test_symbol(self):
        id1 = uuid.uuid4()
        id2 = uuid.uuid4()

        s1 = gtirb.Symbol(name="name", payload=None, uuid=id1)
        s2 = gtirb.Symbol(name="name", payload=None, uuid=id1)
        self.assertTrue(s1.deep_eq(s2))

        s1 = gtirb.Symbol(name="name", payload=5, uuid=id1)
        s2 = gtirb.Symbol(name="name", payload=5, uuid=id1)
        self.assertTrue(s1.deep_eq(s2))

        s1 = gtirb.Symbol(
            name="name",
            payload=gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id1),
            uuid=id1,
        )
        s2 = gtirb.Symbol(
            name="name",
            payload=gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id1),
            uuid=id1,
        )
        self.assertTrue(s1.deep_eq(s2))

        s1 = gtirb.Symbol(name="name1", payload=None, uuid=id1)
        s2 = gtirb.Symbol(name="name2", payload=None, uuid=id1)
        self.assertFalse(s1.deep_eq(s2))

        s1 = gtirb.Symbol(name="name", payload=None, uuid=id1)
        s2 = gtirb.Symbol(name="name", payload=5, uuid=id1)
        self.assertFalse(s1.deep_eq(s2))

        s1 = gtirb.Symbol(
            name="name",
            payload=gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id1),
            uuid=id1,
        )
        s2 = gtirb.Symbol(
            name="name",
            payload=gtirb.CodeBlock(size=2, decode_mode=2, offset=3, uuid=id1),
            uuid=id1,
        )
        self.assertFalse(s1.deep_eq(s2))

        s1 = gtirb.Symbol(name="name", payload=None, uuid=id1)
        s2 = gtirb.Symbol(name="name", payload=None, uuid=id2)
        self.assertFalse(s1.deep_eq(s2))

    def test_sym_exprs(self):
        id1 = uuid.uuid4()
        id2 = uuid.uuid4()

        # SymAddrConst
        s1 = gtirb.SymAddrConst(
            offset=1,
            symbol=gtirb.Symbol(name="name", payload=None, uuid=id1),
            attributes={gtirb.SymbolicExpression.Attribute.G1},
        )
        s2 = gtirb.SymAddrConst(
            offset=1,
            symbol=gtirb.Symbol(name="name", payload=None, uuid=id1),
            attributes={gtirb.SymbolicExpression.Attribute.G1},
        )
        self.assertTrue(s1.deep_eq(s2))

        s1 = gtirb.SymAddrConst(
            offset=1, symbol=gtirb.Symbol(name="name", payload=None, uuid=id1)
        )
        s2 = gtirb.SymAddrConst(
            offset=2, symbol=gtirb.Symbol(name="name", payload=None, uuid=id1)
        )
        self.assertFalse(s1.deep_eq(s2))

        s1 = gtirb.SymAddrConst(
            offset=1, symbol=gtirb.Symbol(name="name1", payload=None, uuid=id1)
        )
        s2 = gtirb.SymAddrConst(
            offset=1, symbol=gtirb.Symbol(name="name2", payload=None, uuid=id1)
        )
        self.assertFalse(s1.deep_eq(s2))

        s1 = gtirb.SymAddrConst(
            offset=1,
            symbol=gtirb.Symbol(name="name", payload=None, uuid=id1),
            attributes={gtirb.SymbolicExpression.Attribute.G1},
        )
        s2 = gtirb.SymAddrConst(
            offset=1,
            symbol=gtirb.Symbol(name="name", payload=None, uuid=id1),
        )
        self.assertFalse(s1.deep_eq(s2))

        # SymAddrAddr
        s1 = gtirb.SymAddrAddr(
            offset=1,
            scale=2,
            symbol1=gtirb.Symbol(name="name1", payload=None, uuid=id1),
            symbol2=gtirb.Symbol(name="name2", payload=None, uuid=id2),
            attributes={gtirb.SymbolicExpression.Attribute.G1},
        )
        s2 = gtirb.SymAddrAddr(
            offset=1,
            scale=2,
            symbol1=gtirb.Symbol(name="name1", payload=None, uuid=id1),
            symbol2=gtirb.Symbol(name="name2", payload=None, uuid=id2),
            attributes={gtirb.SymbolicExpression.Attribute.G1},
        )
        self.assertTrue(s1.deep_eq(s2))

        s1 = gtirb.SymAddrAddr(
            offset=1,
            scale=2,
            symbol1=gtirb.Symbol(name="name1", payload=None, uuid=id1),
            symbol2=gtirb.Symbol(name="name2", payload=None, uuid=id2),
        )
        s2 = gtirb.SymAddrAddr(
            offset=2,
            scale=2,
            symbol1=gtirb.Symbol(name="name1", payload=None, uuid=id1),
            symbol2=gtirb.Symbol(name="name2", payload=None, uuid=id2),
        )
        self.assertFalse(s1.deep_eq(s2))

        s1 = gtirb.SymAddrAddr(
            offset=1,
            scale=2,
            symbol1=gtirb.Symbol(name="name1", payload=None, uuid=id1),
            symbol2=gtirb.Symbol(name="name2", payload=None, uuid=id2),
        )
        s2 = gtirb.SymAddrAddr(
            offset=1,
            scale=4,
            symbol1=gtirb.Symbol(name="name1", payload=None, uuid=id1),
            symbol2=gtirb.Symbol(name="name2", payload=None, uuid=id2),
        )
        self.assertFalse(s1.deep_eq(s2))

        s1 = gtirb.SymAddrAddr(
            offset=1,
            scale=2,
            symbol1=gtirb.Symbol(name="name1", payload=None, uuid=id1),
            symbol2=gtirb.Symbol(name="name2", payload=None, uuid=id2),
        )
        s2 = gtirb.SymAddrAddr(
            offset=1,
            scale=2,
            symbol1=gtirb.Symbol(name="name3", payload=None, uuid=id1),
            symbol2=gtirb.Symbol(name="name2", payload=None, uuid=id2),
        )
        self.assertFalse(s1.deep_eq(s2))

        s1 = gtirb.SymAddrAddr(
            offset=1,
            scale=2,
            symbol1=gtirb.Symbol(name="name1", payload=None, uuid=id1),
            symbol2=gtirb.Symbol(name="name2", payload=None, uuid=id2),
        )
        s2 = gtirb.SymAddrAddr(
            offset=1,
            scale=2,
            symbol1=gtirb.Symbol(name="name1", payload=None, uuid=id1),
            symbol2=gtirb.Symbol(name="name3", payload=None, uuid=id2),
        )
        self.assertFalse(s1.deep_eq(s2))

        s1 = gtirb.SymAddrAddr(
            offset=1,
            scale=2,
            symbol1=gtirb.Symbol(name="name1", payload=None, uuid=id1),
            symbol2=gtirb.Symbol(name="name2", payload=None, uuid=id2),
            attributes={gtirb.SymbolicExpression.Attribute.G1},
        )
        s2 = gtirb.SymAddrAddr(
            offset=1,
            scale=2,
            symbol1=gtirb.Symbol(name="name1", payload=None, uuid=id1),
            symbol2=gtirb.Symbol(name="name2", payload=None, uuid=id2),
        )
        self.assertFalse(s1.deep_eq(s2))

    def test_byte_intervals(self):
        id1 = uuid.uuid4()
        id2 = uuid.uuid4()
        id3 = uuid.uuid4()
        id4 = uuid.uuid4()
        id6 = uuid.uuid4()

        b1 = gtirb.ByteInterval(
            address=1,
            contents=b"abcd",
            size=4,
            initialized_size=4,
            blocks=(
                gtirb.DataBlock(size=1, offset=3, uuid=id2),
                gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id3),
            ),
            symbolic_expressions={
                2: gtirb.SymAddrConst(
                    3, gtirb.Symbol(name="name1", payload=4, uuid=id4)
                ),
            },
            uuid=id1,
        )
        b2 = gtirb.ByteInterval(
            address=1,
            contents=b"abcd",
            size=4,
            initialized_size=4,
            blocks=(
                gtirb.DataBlock(size=1, offset=3, uuid=id2),
                gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id3),
            ),
            symbolic_expressions={
                2: gtirb.SymAddrConst(
                    3, gtirb.Symbol(name="name1", payload=4, uuid=id4)
                ),
            },
            uuid=id1,
        )
        self.assertTrue(b1.deep_eq(b2))

        b2 = gtirb.ByteInterval(
            address=None,
            contents=b"abcd",
            size=4,
            initialized_size=4,
            blocks=(
                gtirb.DataBlock(size=1, offset=3, uuid=id2),
                gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id3),
            ),
            symbolic_expressions={
                2: gtirb.SymAddrConst(
                    3, gtirb.Symbol(name="name1", payload=4, uuid=id4)
                ),
            },
            uuid=id1,
        )
        self.assertFalse(b1.deep_eq(b2))

        b2 = gtirb.ByteInterval(
            address=1,
            contents=b"1234",
            size=4,
            initialized_size=4,
            blocks=(
                gtirb.DataBlock(size=1, offset=3, uuid=id2),
                gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id3),
            ),
            symbolic_expressions={
                2: gtirb.SymAddrConst(
                    3, gtirb.Symbol(name="name1", payload=4, uuid=id4)
                ),
            },
            uuid=id1,
        )
        self.assertFalse(b1.deep_eq(b2))

        b2 = gtirb.ByteInterval(
            address=1,
            contents=b"abcd",
            size=8,
            initialized_size=4,
            blocks=(
                gtirb.DataBlock(size=1, offset=3, uuid=id2),
                gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id3),
            ),
            symbolic_expressions={
                2: gtirb.SymAddrConst(
                    3, gtirb.Symbol(name="name1", payload=4, uuid=id4)
                ),
            },
            uuid=id1,
        )
        self.assertFalse(b1.deep_eq(b2))

        b2 = gtirb.ByteInterval(
            address=1,
            contents=b"abcd",
            size=4,
            initialized_size=0,
            blocks=(
                gtirb.DataBlock(size=1, offset=3, uuid=id2),
                gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id3),
            ),
            symbolic_expressions={
                2: gtirb.SymAddrConst(
                    3, gtirb.Symbol(name="name1", payload=4, uuid=id4)
                ),
            },
            uuid=id1,
        )
        self.assertFalse(b1.deep_eq(b2))

        b2 = gtirb.ByteInterval(
            address=1,
            contents=b"abcd",
            size=4,
            initialized_size=4,
            blocks=(
                gtirb.DataBlock(size=1, offset=3, uuid=id2),
                gtirb.CodeBlock(size=1, decode_mode=5, offset=3, uuid=id3),
            ),
            symbolic_expressions={
                2: gtirb.SymAddrConst(
                    3, gtirb.Symbol(name="name1", payload=4, uuid=id4)
                ),
            },
            uuid=id1,
        )
        self.assertFalse(b1.deep_eq(b2))

        b2 = gtirb.ByteInterval(
            address=1,
            contents=b"abcd",
            size=4,
            initialized_size=4,
            blocks=(
                gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id3),
            ),
            symbolic_expressions={
                2: gtirb.SymAddrConst(
                    3, gtirb.Symbol(name="name1", payload=4, uuid=id4)
                ),
            },
            uuid=id1,
        )
        self.assertFalse(b1.deep_eq(b2))

        b2 = gtirb.ByteInterval(
            address=1,
            contents=b"abcd",
            size=4,
            initialized_size=4,
            blocks=(
                gtirb.DataBlock(size=1, offset=3, uuid=id2),
                gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id3),
            ),
            symbolic_expressions={
                2: gtirb.SymAddrConst(
                    6, gtirb.Symbol(name="name1", payload=4, uuid=id4)
                ),
            },
            uuid=id1,
        )
        self.assertFalse(b1.deep_eq(b2))

        b2 = gtirb.ByteInterval(
            address=1,
            contents=b"abcd",
            size=4,
            initialized_size=4,
            blocks=(
                gtirb.DataBlock(size=1, offset=3, uuid=id2),
                gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id3),
            ),
            symbolic_expressions={
                2: gtirb.SymAddrConst(
                    3, gtirb.Symbol(name="name1", payload=4, uuid=id4)
                )
            },
            uuid=id1,
        )
        self.assertTrue(b1.deep_eq(b2))

        b2 = gtirb.ByteInterval(
            address=1,
            contents=b"abcd",
            size=4,
            initialized_size=4,
            blocks=(
                gtirb.DataBlock(size=1, offset=3, uuid=id2),
                gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id3),
            ),
            symbolic_expressions={
                7: gtirb.SymAddrConst(
                    3, gtirb.Symbol(name="name1", payload=4, uuid=id4)
                ),
            },
            uuid=id1,
        )
        self.assertFalse(b1.deep_eq(b2))

        b2 = gtirb.ByteInterval(
            address=1,
            contents=b"abcd",
            size=4,
            initialized_size=4,
            blocks=(
                gtirb.DataBlock(size=1, offset=3, uuid=id2),
                gtirb.CodeBlock(size=1, decode_mode=2, offset=3, uuid=id3),
            ),
            symbolic_expressions={
                2: gtirb.SymAddrConst(
                    3, gtirb.Symbol(name="name1", payload=4, uuid=id4)
                ),
            },
            uuid=id6,
        )
        self.assertFalse(b1.deep_eq(b2))

    def test_sections(self):
        id1 = uuid.uuid4()
        id2 = uuid.uuid4()
        id3 = uuid.uuid4()
        id4 = uuid.uuid4()

        s1 = gtirb.Section(
            name="name",
            byte_intervals=(
                gtirb.ByteInterval(contents=b"abcd", uuid=id2),
                gtirb.ByteInterval(contents=b"1234", uuid=id3),
            ),
            flags=(gtirb.Section.Flag.Readable, gtirb.Section.Flag.Writable),
            uuid=id1,
        )
        s2 = gtirb.Section(
            name="name",
            byte_intervals=(
                gtirb.ByteInterval(contents=b"abcd", uuid=id2),
                gtirb.ByteInterval(contents=b"1234", uuid=id3),
            ),
            flags=(gtirb.Section.Flag.Readable, gtirb.Section.Flag.Writable),
            uuid=id1,
        )
        self.assertTrue(s1.deep_eq(s2))

        s2 = gtirb.Section(
            name="name2",
            byte_intervals=(
                gtirb.ByteInterval(contents=b"abcd", uuid=id2),
                gtirb.ByteInterval(contents=b"1234", uuid=id3),
            ),
            flags=(gtirb.Section.Flag.Readable, gtirb.Section.Flag.Writable),
            uuid=id1,
        )
        self.assertFalse(s1.deep_eq(s2))

        s2 = gtirb.Section(
            name="name",
            byte_intervals=(
                gtirb.ByteInterval(contents=b"abcd", uuid=id2),
                gtirb.ByteInterval(contents=b"12345", uuid=id3),
            ),
            flags=(gtirb.Section.Flag.Readable, gtirb.Section.Flag.Writable),
            uuid=id1,
        )
        self.assertFalse(s1.deep_eq(s2))

        s2 = gtirb.Section(
            name="name",
            byte_intervals=(gtirb.ByteInterval(contents=b"abcd", uuid=id2),),
            flags=(gtirb.Section.Flag.Readable, gtirb.Section.Flag.Writable),
            uuid=id1,
        )
        self.assertFalse(s1.deep_eq(s2))

        s2 = gtirb.Section(
            name="name",
            byte_intervals=(
                gtirb.ByteInterval(contents=b"abcd", uuid=id2),
                gtirb.ByteInterval(contents=b"1234", uuid=id3),
            ),
            flags=(gtirb.Section.Flag.Writable,),
            uuid=id1,
        )
        self.assertFalse(s1.deep_eq(s2))

        s2 = gtirb.Section(
            name="name",
            byte_intervals=(
                gtirb.ByteInterval(contents=b"abcd", uuid=id2),
                gtirb.ByteInterval(contents=b"1234", uuid=id3),
            ),
            flags=(
                gtirb.Section.Flag.Readable,
                gtirb.Section.Flag.Writable,
                gtirb.Section.Flag.Loaded,
            ),
            uuid=id1,
        )
        self.assertFalse(s1.deep_eq(s2))

        s2 = gtirb.Section(
            name="name",
            byte_intervals=(
                gtirb.ByteInterval(contents=b"abcd", uuid=id2),
                gtirb.ByteInterval(contents=b"1234", uuid=id3),
            ),
            flags=(gtirb.Section.Flag.Readable, gtirb.Section.Flag.Writable),
            uuid=id4,
        )
        self.assertFalse(s1.deep_eq(s2))

    def test_cfg(self):
        id1 = uuid.uuid4()
        id2 = uuid.uuid4()

        e1 = gtirb.CFG(
            [
                gtirb.Edge(
                    gtirb.CodeBlock(size=1, uuid=id1),
                    gtirb.CodeBlock(size=2, uuid=id2),
                    gtirb.Edge.Label(
                        type=gtirb.Edge.Type.Branch,
                        conditional=True,
                        direct=False,
                    ),
                )
            ]
        )
        self.assertFalse(
            e1.deep_eq(
                [
                    gtirb.Edge(
                        gtirb.CodeBlock(size=1, uuid=id1),
                        gtirb.CodeBlock(size=2, uuid=id2),
                        gtirb.Edge.Label(
                            type=gtirb.Edge.Type.Branch,
                            conditional=True,
                            direct=False,
                        ),
                    )
                ]
            )
        )

        e2 = gtirb.CFG(
            [
                gtirb.Edge(
                    gtirb.CodeBlock(size=1, uuid=id1),
                    gtirb.CodeBlock(size=2, uuid=id2),
                    gtirb.Edge.Label(
                        type=gtirb.Edge.Type.Branch,
                        conditional=True,
                        direct=False,
                    ),
                )
            ]
        )
        self.assertTrue(e1.deep_eq(e2))

        e2 = gtirb.CFG(
            [
                gtirb.Edge(
                    gtirb.CodeBlock(size=3, uuid=id1),
                    gtirb.CodeBlock(size=2, uuid=id2),
                    gtirb.Edge.Label(
                        type=gtirb.Edge.Type.Branch,
                        conditional=True,
                        direct=False,
                    ),
                )
            ]
        )
        self.assertFalse(e1.deep_eq(e2))

        e2 = gtirb.CFG(
            [
                gtirb.Edge(
                    gtirb.CodeBlock(size=1, uuid=id1),
                    gtirb.CodeBlock(size=3, uuid=id2),
                    gtirb.Edge.Label(
                        type=gtirb.Edge.Type.Branch,
                        conditional=True,
                        direct=False,
                    ),
                )
            ]
        )
        self.assertFalse(e1.deep_eq(e2))

        e2 = gtirb.CFG(
            [
                gtirb.Edge(
                    gtirb.CodeBlock(size=1, uuid=id1),
                    gtirb.CodeBlock(size=2, uuid=id2),
                    gtirb.Edge.Label(
                        type=gtirb.Edge.Type.Fallthrough,
                        conditional=True,
                        direct=False,
                    ),
                )
            ]
        )
        self.assertFalse(e1.deep_eq(e2))

        e2 = gtirb.CFG(
            [
                gtirb.Edge(
                    gtirb.CodeBlock(size=1, uuid=id1),
                    gtirb.CodeBlock(size=2, uuid=id2),
                    gtirb.Edge.Label(
                        type=gtirb.Edge.Type.Branch,
                        conditional=False,
                        direct=False,
                    ),
                )
            ]
        )
        self.assertFalse(e1.deep_eq(e2))

        e2 = gtirb.CFG(
            [
                gtirb.Edge(
                    gtirb.CodeBlock(size=1, uuid=id1),
                    gtirb.CodeBlock(size=2, uuid=id2),
                    gtirb.Edge.Label(
                        type=gtirb.Edge.Type.Branch,
                        conditional=True,
                        direct=True,
                    ),
                )
            ]
        )
        self.assertFalse(e1.deep_eq(e2))

    def test_module(self):
        id1 = uuid.uuid4()
        id2 = uuid.uuid4()
        id3 = uuid.uuid4()
        id4 = uuid.uuid4()
        id5 = uuid.uuid4()
        id6 = uuid.uuid4()
        id7 = uuid.uuid4()
        id8 = uuid.uuid4()

        m1 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("value", "string")},
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.ELF,
            isa=gtirb.Module.ISA.X64,
            name="name",
            preferred_addr=1,
            rebase_delta=2,
            entry_point=gtirb.CodeBlock(size=1, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id3), gtirb.ProxyBlock(uuid=id4)),
            symbols=(
                gtirb.Symbol(name="sym1", uuid=id5),
                gtirb.Symbol(name="sym2", uuid=id6),
            ),
            sections=(
                gtirb.Section(name="sect1", uuid=id7),
                gtirb.Section(name="sect2", uuid=id8),
            ),
            uuid=id1,
        )
        m2 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("value", "string")},
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.ELF,
            isa=gtirb.Module.ISA.X64,
            name="name",
            preferred_addr=1,
            rebase_delta=2,
            entry_point=gtirb.CodeBlock(size=1, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id3), gtirb.ProxyBlock(uuid=id4)),
            symbols=(
                gtirb.Symbol(name="sym1", uuid=id5),
                gtirb.Symbol(name="sym2", uuid=id6),
            ),
            sections=(
                gtirb.Section(name="sect1", uuid=id7),
                gtirb.Section(name="sect2", uuid=id8),
            ),
            uuid=id1,
        )
        self.assertTrue(m1.deep_eq(m2))

        m2 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("other_value", "string")},
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.ELF,
            isa=gtirb.Module.ISA.X64,
            name="name",
            preferred_addr=1,
            rebase_delta=2,
            entry_point=gtirb.CodeBlock(size=1, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id3), gtirb.ProxyBlock(uuid=id4)),
            symbols=(
                gtirb.Symbol(name="sym1", uuid=id5),
                gtirb.Symbol(name="sym2", uuid=id6),
            ),
            sections=(
                gtirb.Section(name="sect1", uuid=id7),
                gtirb.Section(name="sect2", uuid=id8),
            ),
            uuid=id1,
        )
        self.assertTrue(m1.deep_eq(m2))

        m2 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("value", "string")},
            binary_path="other_binary_path",
            file_format=gtirb.Module.FileFormat.ELF,
            isa=gtirb.Module.ISA.X64,
            name="name",
            preferred_addr=1,
            rebase_delta=2,
            entry_point=gtirb.CodeBlock(size=1, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id3), gtirb.ProxyBlock(uuid=id4)),
            symbols=(
                gtirb.Symbol(name="sym1", uuid=id5),
                gtirb.Symbol(name="sym2", uuid=id6),
            ),
            sections=(
                gtirb.Section(name="sect1", uuid=id7),
                gtirb.Section(name="sect2", uuid=id8),
            ),
            uuid=id1,
        )
        self.assertFalse(m1.deep_eq(m2))

        m2 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("value", "string")},
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.PE,
            isa=gtirb.Module.ISA.X64,
            name="name",
            preferred_addr=1,
            rebase_delta=2,
            entry_point=gtirb.CodeBlock(size=1, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id3), gtirb.ProxyBlock(uuid=id4)),
            symbols=(
                gtirb.Symbol(name="sym1", uuid=id5),
                gtirb.Symbol(name="sym2", uuid=id6),
            ),
            sections=(
                gtirb.Section(name="sect1", uuid=id7),
                gtirb.Section(name="sect2", uuid=id8),
            ),
            uuid=id1,
        )
        self.assertFalse(m1.deep_eq(m2))

        m2 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("value", "string")},
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.ELF,
            isa=gtirb.Module.ISA.ARM,
            name="name",
            preferred_addr=1,
            rebase_delta=2,
            entry_point=gtirb.CodeBlock(size=1, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id3), gtirb.ProxyBlock(uuid=id4)),
            symbols=(
                gtirb.Symbol(name="sym1", uuid=id5),
                gtirb.Symbol(name="sym2", uuid=id6),
            ),
            sections=(
                gtirb.Section(name="sect1", uuid=id7),
                gtirb.Section(name="sect2", uuid=id8),
            ),
            uuid=id1,
        )
        self.assertFalse(m1.deep_eq(m2))

        m2 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("value", "string")},
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.ELF,
            isa=gtirb.Module.ISA.X64,
            name="other_name",
            preferred_addr=1,
            rebase_delta=2,
            entry_point=gtirb.CodeBlock(size=1, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id3), gtirb.ProxyBlock(uuid=id4)),
            symbols=(
                gtirb.Symbol(name="sym1", uuid=id5),
                gtirb.Symbol(name="sym2", uuid=id6),
            ),
            sections=(
                gtirb.Section(name="sect1", uuid=id7),
                gtirb.Section(name="sect2", uuid=id8),
            ),
            uuid=id1,
        )
        self.assertFalse(m1.deep_eq(m2))

        m2 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("value", "string")},
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.ELF,
            isa=gtirb.Module.ISA.X64,
            name="name",
            preferred_addr=5,
            rebase_delta=2,
            entry_point=gtirb.CodeBlock(size=1, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id3), gtirb.ProxyBlock(uuid=id4)),
            symbols=(
                gtirb.Symbol(name="sym1", uuid=id5),
                gtirb.Symbol(name="sym2", uuid=id6),
            ),
            sections=(
                gtirb.Section(name="sect1", uuid=id7),
                gtirb.Section(name="sect2", uuid=id8),
            ),
            uuid=id1,
        )
        self.assertFalse(m1.deep_eq(m2))

        m2 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("value", "string")},
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.ELF,
            isa=gtirb.Module.ISA.X64,
            name="name",
            preferred_addr=1,
            rebase_delta=5,
            entry_point=gtirb.CodeBlock(size=1, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id3), gtirb.ProxyBlock(uuid=id4)),
            symbols=(
                gtirb.Symbol(name="sym1", uuid=id5),
                gtirb.Symbol(name="sym2", uuid=id6),
            ),
            sections=(
                gtirb.Section(name="sect1", uuid=id7),
                gtirb.Section(name="sect2", uuid=id8),
            ),
            uuid=id1,
        )
        self.assertFalse(m1.deep_eq(m2))

        m2 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("value", "string")},
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.ELF,
            isa=gtirb.Module.ISA.X64,
            name="name",
            preferred_addr=1,
            rebase_delta=2,
            entry_point=gtirb.CodeBlock(size=2, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id3), gtirb.ProxyBlock(uuid=id4)),
            symbols=(
                gtirb.Symbol(name="sym1", uuid=id5),
                gtirb.Symbol(name="sym2", uuid=id6),
            ),
            sections=(
                gtirb.Section(name="sect1", uuid=id7),
                gtirb.Section(name="sect2", uuid=id8),
            ),
            uuid=id1,
        )
        self.assertFalse(m1.deep_eq(m2))

        m2 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("value", "string")},
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.ELF,
            isa=gtirb.Module.ISA.X64,
            name="name",
            preferred_addr=1,
            rebase_delta=2,
            entry_point=gtirb.CodeBlock(size=1, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id4), gtirb.ProxyBlock(uuid=id4)),
            symbols=(
                gtirb.Symbol(name="sym1", uuid=id5),
                gtirb.Symbol(name="sym2", uuid=id6),
            ),
            sections=(
                gtirb.Section(name="sect1", uuid=id7),
                gtirb.Section(name="sect2", uuid=id8),
            ),
            uuid=id1,
        )
        self.assertFalse(m1.deep_eq(m2))

        m2 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("value", "string")},
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.ELF,
            isa=gtirb.Module.ISA.X64,
            name="name",
            preferred_addr=1,
            rebase_delta=2,
            entry_point=gtirb.CodeBlock(size=1, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id4),),
            symbols=(
                gtirb.Symbol(name="sym1", uuid=id5),
                gtirb.Symbol(name="sym2", uuid=id6),
            ),
            sections=(
                gtirb.Section(name="sect1", uuid=id7),
                gtirb.Section(name="sect2", uuid=id8),
            ),
            uuid=id1,
        )
        self.assertFalse(m1.deep_eq(m2))

        m2 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("value", "string")},
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.ELF,
            isa=gtirb.Module.ISA.X64,
            name="name",
            preferred_addr=1,
            rebase_delta=2,
            entry_point=gtirb.CodeBlock(size=1, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id3), gtirb.ProxyBlock(uuid=id4)),
            symbols=(
                gtirb.Symbol(name="sym11", uuid=id5),
                gtirb.Symbol(name="sym2", uuid=id6),
            ),
            sections=(
                gtirb.Section(name="sect1", uuid=id7),
                gtirb.Section(name="sect2", uuid=id8),
            ),
            uuid=id1,
        )
        self.assertFalse(m1.deep_eq(m2))

        m2 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("value", "string")},
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.ELF,
            isa=gtirb.Module.ISA.X64,
            name="name",
            preferred_addr=1,
            rebase_delta=2,
            entry_point=gtirb.CodeBlock(size=1, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id3), gtirb.ProxyBlock(uuid=id4)),
            symbols=(gtirb.Symbol(name="sym1", uuid=id5),),
            sections=(
                gtirb.Section(name="sect1", uuid=id7),
                gtirb.Section(name="sect2", uuid=id8),
            ),
            uuid=id1,
        )
        self.assertFalse(m1.deep_eq(m2))

        m2 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("value", "string")},
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.ELF,
            isa=gtirb.Module.ISA.X64,
            name="name",
            preferred_addr=1,
            rebase_delta=2,
            entry_point=gtirb.CodeBlock(size=1, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id3), gtirb.ProxyBlock(uuid=id4)),
            symbols=(
                gtirb.Symbol(name="sym1", uuid=id5),
                gtirb.Symbol(name="sym2", uuid=id6),
            ),
            sections=(
                gtirb.Section(name="sect1", uuid=id7),
                gtirb.Section(name="sect22", uuid=id8),
            ),
            uuid=id1,
        )
        self.assertFalse(m1.deep_eq(m2))

        m2 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("value", "string")},
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.ELF,
            isa=gtirb.Module.ISA.X64,
            name="name",
            preferred_addr=1,
            rebase_delta=2,
            entry_point=gtirb.CodeBlock(size=1, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id3), gtirb.ProxyBlock(uuid=id4)),
            symbols=(
                gtirb.Symbol(name="sym1", uuid=id5),
                gtirb.Symbol(name="sym2", uuid=id6),
            ),
            sections=(gtirb.Section(name="sect2", uuid=id8),),
            uuid=id1,
        )
        self.assertFalse(m1.deep_eq(m2))

        m2 = gtirb.Module(
            aux_data={"key": gtirb.AuxData("value", "string")},
            binary_path="binary_path",
            file_format=gtirb.Module.FileFormat.ELF,
            isa=gtirb.Module.ISA.X64,
            name="name",
            preferred_addr=1,
            rebase_delta=2,
            entry_point=gtirb.CodeBlock(size=1, uuid=id2),
            proxies=(gtirb.ProxyBlock(uuid=id3), gtirb.ProxyBlock(uuid=id4)),
            symbols=(
                gtirb.Symbol(name="sym1", uuid=id5),
                gtirb.Symbol(name="sym2", uuid=id6),
            ),
            sections=(
                gtirb.Section(name="sect1", uuid=id7),
                gtirb.Section(name="sect2", uuid=id8),
            ),
            uuid=id2,
        )
        self.assertFalse(m1.deep_eq(m2))

    def test_ir(self):
        id1 = uuid.uuid4()
        id2 = uuid.uuid4()
        id3 = uuid.uuid4()
        id4 = uuid.uuid4()
        id5 = uuid.uuid4()
        id6 = uuid.uuid4()
        id7 = uuid.uuid4()
        id8 = uuid.uuid4()

        ir1 = gtirb.IR(
            modules=(
                gtirb.Module(name="m1", uuid=id2),
                gtirb.Module(name="m2", uuid=id3),
            ),
            aux_data={"key": gtirb.AuxData("value", "string")},
            cfg=(
                gtirb.Edge(
                    gtirb.CodeBlock(size=1, uuid=id4),
                    gtirb.CodeBlock(size=2, uuid=id5),
                ),
                gtirb.Edge(
                    gtirb.CodeBlock(size=3, uuid=id6),
                    gtirb.CodeBlock(size=4, uuid=id7),
                ),
            ),
            version=1,
            uuid=id1,
        )
        ir2 = gtirb.IR(
            modules=(
                gtirb.Module(name="m1", uuid=id2),
                gtirb.Module(name="m2", uuid=id3),
            ),
            aux_data={"key": gtirb.AuxData("value", "string")},
            cfg=(
                gtirb.Edge(
                    gtirb.CodeBlock(size=1, uuid=id4),
                    gtirb.CodeBlock(size=2, uuid=id5),
                ),
                gtirb.Edge(
                    gtirb.CodeBlock(size=3, uuid=id6),
                    gtirb.CodeBlock(size=4, uuid=id7),
                ),
            ),
            version=1,
            uuid=id1,
        )
        self.assertTrue(ir1.deep_eq(ir2))

        ir2 = gtirb.IR(
            modules=(
                gtirb.Module(name="m1", uuid=id2),
                gtirb.Module(name="m2", uuid=id3),
            ),
            aux_data={"key": gtirb.AuxData("other_value", "string")},
            cfg=(
                gtirb.Edge(
                    gtirb.CodeBlock(size=1, uuid=id4),
                    gtirb.CodeBlock(size=2, uuid=id5),
                ),
                gtirb.Edge(
                    gtirb.CodeBlock(size=3, uuid=id6),
                    gtirb.CodeBlock(size=4, uuid=id7),
                ),
            ),
            version=1,
            uuid=id1,
        )
        self.assertTrue(ir1.deep_eq(ir2))

        ir2 = gtirb.IR(
            modules=(
                gtirb.Module(name="m11", uuid=id2),
                gtirb.Module(name="m2", uuid=id3),
            ),
            aux_data={"key": gtirb.AuxData("value", "string")},
            cfg=(
                gtirb.Edge(
                    gtirb.CodeBlock(size=1, uuid=id4),
                    gtirb.CodeBlock(size=2, uuid=id5),
                ),
                gtirb.Edge(
                    gtirb.CodeBlock(size=3, uuid=id6),
                    gtirb.CodeBlock(size=4, uuid=id7),
                ),
            ),
            version=1,
            uuid=id1,
        )
        self.assertFalse(ir1.deep_eq(ir2))

        ir2 = gtirb.IR(
            modules=(gtirb.Module(name="m1", uuid=id2),),
            aux_data={"key": gtirb.AuxData("value", "string")},
            cfg=(
                gtirb.Edge(
                    gtirb.CodeBlock(size=1, uuid=id4),
                    gtirb.CodeBlock(size=2, uuid=id5),
                ),
                gtirb.Edge(
                    gtirb.CodeBlock(size=3, uuid=id6),
                    gtirb.CodeBlock(size=4, uuid=id7),
                ),
            ),
            version=1,
            uuid=id1,
        )
        self.assertFalse(ir1.deep_eq(ir2))

        ir2 = gtirb.IR(
            modules=(
                gtirb.Module(name="m1", uuid=id2),
                gtirb.Module(name="m2", uuid=id3),
            ),
            aux_data={"key": gtirb.AuxData("value", "string")},
            cfg=(
                gtirb.Edge(
                    gtirb.CodeBlock(size=55, uuid=id4),
                    gtirb.CodeBlock(size=2, uuid=id5),
                ),
                gtirb.Edge(
                    gtirb.CodeBlock(size=3, uuid=id6),
                    gtirb.CodeBlock(size=4, uuid=id7),
                ),
            ),
            version=1,
            uuid=id1,
        )
        self.assertFalse(ir1.deep_eq(ir2))

        ir2 = gtirb.IR(
            modules=(
                gtirb.Module(name="m1", uuid=id2),
                gtirb.Module(name="m2", uuid=id3),
            ),
            aux_data={"key": gtirb.AuxData("value", "string")},
            cfg=(
                gtirb.Edge(
                    gtirb.CodeBlock(size=3, uuid=id6),
                    gtirb.CodeBlock(size=4, uuid=id7),
                ),
            ),
            version=1,
            uuid=id1,
        )
        self.assertFalse(ir1.deep_eq(ir2))

        ir2 = gtirb.IR(
            modules=(
                gtirb.Module(name="m1", uuid=id2),
                gtirb.Module(name="m2", uuid=id3),
            ),
            aux_data={"key": gtirb.AuxData("value", "string")},
            cfg=(
                gtirb.Edge(
                    gtirb.CodeBlock(size=1, uuid=id4),
                    gtirb.CodeBlock(size=2, uuid=id5),
                ),
                gtirb.Edge(
                    gtirb.CodeBlock(size=3, uuid=id6),
                    gtirb.CodeBlock(size=4, uuid=id7),
                ),
            ),
            version=5,
            uuid=id1,
        )
        self.assertFalse(ir1.deep_eq(ir2))

        ir2 = gtirb.IR(
            modules=(
                gtirb.Module(name="m1", uuid=id2),
                gtirb.Module(name="m2", uuid=id3),
            ),
            aux_data={"key": gtirb.AuxData("value", "string")},
            cfg=(
                gtirb.Edge(
                    gtirb.CodeBlock(size=1, uuid=id4),
                    gtirb.CodeBlock(size=2, uuid=id5),
                ),
                gtirb.Edge(
                    gtirb.CodeBlock(size=3, uuid=id6),
                    gtirb.CodeBlock(size=4, uuid=id7),
                ),
            ),
            version=1,
            uuid=id8,
        )
        self.assertFalse(ir1.deep_eq(ir2))


if __name__ == "__main__":
    unittest.main()
