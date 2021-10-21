import io
import unittest

import gtirb
from helpers import create_interval_etc


class SymbolicExpressionAttributes(unittest.TestCase):
    def test_unknown_attributes(self):
        ir, m, s, bi = create_interval_etc(address=0x1000, size=5)
        sym = gtirb.Symbol(name="foo")
        m.symbols.add(sym)
        expr = gtirb.SymAddrConst(
            0, sym, {gtirb.SymbolicExpression.Attribute.GOT, 0xBEEF}
        )
        bi.symbolic_expressions[0] = expr

        out = io.BytesIO()
        ir.save_protobuf_file(out)
        out.seek(0)

        ir = gtirb.IR.load_protobuf_file(out)
        bi = next(ir.byte_intervals)
        expr = bi.symbolic_expressions[0]

        self.assertTrue(
            gtirb.SymbolicExpression.Attribute.GOT in expr.attributes
        )
        self.assertTrue(0xBEEF in expr.attributes)
