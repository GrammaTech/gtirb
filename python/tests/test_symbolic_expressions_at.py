import unittest

from helpers import SearchScope, create_interval_etc, parameterize_one

import gtirb


class SymbolicExpressionsAtTests(unittest.TestCase):
    @parameterize_one("scope", list(SearchScope))
    def test_symbolic_expressions_at(self, scope):
        ir, m, s, bi = create_interval_etc(address=0x1000, size=5)
        search_in = scope.select(ir, m, s, bi)

        sym = gtirb.Symbol(name="hello")
        expr = gtirb.SymAddrConst(0, sym)
        bi.symbolic_expressions[1] = expr
        bi.symbolic_expressions[2] = expr
        bi.symbolic_expressions[3] = expr
        bi.symbolic_expressions[4] = expr

        found = set(search_in.symbolic_expressions_at(0x1001))
        self.assertEqual(found, {(bi, 1, expr)})

        found = set(search_in.symbolic_expressions_at(range(0x1000, 0x1004)))
        self.assertEqual(found, {(bi, 1, expr), (bi, 2, expr), (bi, 3, expr)})

        found = set(
            search_in.symbolic_expressions_at(range(0x1000, 0x1004, 2))
        )
        self.assertEqual(found, {(bi, 2, expr)})

        # Now just verify that the index updates correctly when deleting
        del bi.symbolic_expressions[1]

        found = set(search_in.symbolic_expressions_at(0x1001))
        self.assertEqual(found, set())
