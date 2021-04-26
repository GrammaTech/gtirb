"""isort:skip_file"""

import unittest
import gtirb
import uuid

# imports needed because repr's use of unqualified names
from uuid import UUID  # noqa: F401,F403,F401
from gtirb import *  # noqa: F401,F403,F401


class ReprTest(unittest.TestCase):
    def test_auxdata(self):
        node = gtirb.AuxData(
            type_name="mapping<string,set<UUID>>",
            data={
                "a": set([uuid.uuid4(), uuid.uuid4()]),
                "b": set([uuid.uuid4()]),
            },
        )
        string = repr(node)
        new_node = eval(string)
        # auxdata has no deep_eq
        # (because how can we ensure "data" has a deep_eq?)
        self.assertEqual(node.type_name, new_node.type_name)
        self.assertEqual(node.data, new_node.data)

    def test_block(self):
        node = gtirb.CodeBlock(offset=123, size=456, decode_mode=789)
        string = repr(node)
        new_node = eval(string)
        self.assertTrue(node.deep_eq(new_node))

    def test_proxy_block(self):
        node = gtirb.ProxyBlock()
        string = repr(node)
        new_node = eval(string)
        self.assertTrue(node.deep_eq(new_node))

    def test_data_object(self):
        node = gtirb.DataBlock(offset=123, size=456)
        string = repr(node)
        new_node = eval(string)
        self.assertTrue(node.deep_eq(new_node))

    def test_ir(self):
        # TODO: expand this
        node = gtirb.IR()
        string = repr(node)
        new_node = eval(string)
        self.assertTrue(node.deep_eq(new_node))

    def test_cfg(self):
        node = gtirb.CFG()
        node.add(
            gtirb.Edge(
                gtirb.CodeBlock(offset=1, size=2),
                gtirb.CodeBlock(offset=3, size=4),
                gtirb.Edge.Label(
                    type=gtirb.Edge.Type.Fallthrough,
                    conditional=True,
                    direct=False,
                ),
            )
        )
        node.add(
            gtirb.Edge(
                gtirb.CodeBlock(offset=5, size=6),
                gtirb.CodeBlock(offset=7, size=8),
                gtirb.Edge.Label(
                    type=gtirb.Edge.Type.Branch,
                    conditional=True,
                    direct=False,
                ),
            )
        )
        string = repr(node)
        new_node = eval(string)
        self.assertTrue(node.deep_eq(new_node))

    def test_module(self):
        # TODO: expand this
        node = gtirb.Module(name="M")
        string = repr(node)
        new_node = eval(string)
        self.assertTrue(node.deep_eq(new_node))

    def test_offset(self):
        node = gtirb.Offset(element_id=uuid.uuid4(), displacement=123)
        string = repr(node)
        new_node = eval(string)
        self.assertEqual(node.element_id, new_node.element_id)
        self.assertEqual(node.displacement, new_node.displacement)

    def test_section(self):
        node = gtirb.Section(
            name=".text",
            flags=(gtirb.Section.Flag.Readable, gtirb.Section.Flag.Writable),
        )
        string = repr(node)
        new_node = eval(string)
        self.assertTrue(node.deep_eq(new_node))

    def test_symbol(self):
        node = gtirb.Symbol(name="symbol1", payload=gtirb.ProxyBlock())
        string = repr(node)
        new_node = eval(string)
        self.assertTrue(node.deep_eq(new_node))

        node = gtirb.Symbol(name="symbol2", payload=0x123)
        string = repr(node)
        new_node = eval(string)
        self.assertTrue(node.deep_eq(new_node))

        node = gtirb.Symbol(name="symbol3", payload=None)
        string = repr(node)
        new_node = eval(string)
        self.assertTrue(node.deep_eq(new_node))

    def test_sym_expr(self):
        node = gtirb.SymAddrConst(
            offset=123,
            symbol=gtirb.Symbol(name="symbol", payload=gtirb.ProxyBlock()),
            attributes=set(),
        )
        string = repr(node)
        new_node = eval(string)
        self.assertTrue(node.deep_eq(new_node))

        node = gtirb.SymStackConst(
            offset=123,
            symbol=gtirb.Symbol(name="symbol", payload=gtirb.ProxyBlock()),
            attributes={gtirb.SymbolicExpression.Attribute.Adjusted},
        )
        string = repr(node)
        new_node = eval(string)
        self.assertTrue(node.deep_eq(new_node))

        node = gtirb.SymAddrAddr(
            offset=123,
            scale=2,
            symbol1=gtirb.Symbol(name="symbol1", payload=gtirb.ProxyBlock()),
            symbol2=gtirb.Symbol(name="symbol2", payload=gtirb.ProxyBlock()),
            attributes={
                gtirb.SymbolicExpression.Attribute.Adjusted,
                gtirb.SymbolicExpression.Attribute.Part0,
            },
        )
        string = repr(node)
        new_node = eval(string)
        self.assertTrue(node.deep_eq(new_node))

    def test_byte_interval(self):
        node = gtirb.ByteInterval(
            address=0x123,
            initialized_size=456,
            size=789,
            contents=b"abc",
            blocks=(gtirb.DataBlock(size=0),),
            symbolic_expressions={
                1: gtirb.SymAddrConst(offset=1, symbol=gtirb.Symbol("test"))
            },
        )
        string = repr(node)
        new_node = eval(string)
        self.assertTrue(node.deep_eq(new_node))


if __name__ == "__main__":
    unittest.main()
