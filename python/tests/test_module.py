import unittest

import gtirb


class ModuleTests(unittest.TestCase):
    def test_symbols_named(self):
        m = gtirb.Module(name="test")
        s1 = gtirb.Symbol(name="hello", module=m)
        s2 = gtirb.Symbol(name="world", module=m)

        found = set(m.symbols_named("hello"))
        self.assertEqual(found, {s1})

        found = set(m.symbols_named("world"))
        self.assertEqual(found, {s2})

        # Change the name to verify we update the index
        s1.name = "world"
        found = set(m.symbols_named("hello"))
        self.assertEqual(found, set())

        found = set(m.symbols_named("world"))
        self.assertEqual(found, {s1, s2})

        # Discard the symbol to verify we update the index
        m.symbols.discard(s1)
        found = set(m.symbols_named("world"))
        self.assertEqual(found, {s2})

        # Now add it back to verify we update the index
        m.symbols.add(s1)
        found = set(m.symbols_named("world"))
        self.assertEqual(found, {s1, s2})


class SymbolicExpressionsReferencingTests(unittest.TestCase):
    def test_simple(self):
        m = gtirb.Module(name="test")
        s = gtirb.Section(module=m, name=".text")
        sym1 = gtirb.Symbol(module=m, name="sym1")
        sym2 = gtirb.Symbol(module=m, name="sym2")
        sym3 = gtirb.Symbol(module=m, name="sym3")
        bi = gtirb.ByteInterval(
            section=s,
            symbolic_expressions={
                0: gtirb.SymAddrConst(0, sym2),
                1: gtirb.SymAddrAddr(0, 0, sym2, sym3),
            },
        )

        self.assertEqual(set(m.symbolic_expressions_referencing(sym1)), set())
        self.assertEqual(
            set(m.symbolic_expressions_referencing(sym2)),
            {
                (bi, 0, bi.symbolic_expressions[0]),
                (bi, 1, bi.symbolic_expressions[1]),
            },
        )
        self.assertEqual(
            set(m.symbolic_expressions_referencing(sym3)),
            {(bi, 1, bi.symbolic_expressions[1])},
        )

    def test_add_remove_sections(self):
        # Test that adding and removing byte intervals updates the index
        m = gtirb.Module(name="test")
        s = gtirb.Section(name=".text")
        sym = gtirb.Symbol(name="hello", module=m)
        expr = gtirb.SymAddrConst(0, sym)
        bi = gtirb.ByteInterval(symbolic_expressions={1: expr}, section=s)

        self.assertEqual(set(m.symbolic_expressions_referencing(sym)), set())

        s.module = m
        self.assertEqual(
            set(m.symbolic_expressions_referencing(sym)), {(bi, 1, expr)}
        )

        s.module = None
        self.assertEqual(set(m.symbolic_expressions_referencing(sym)), set())

    def test_add_remove_byte_intervals(self):
        # Test that adding and removing byte intervals updates the index
        m = gtirb.Module(name="test")
        s = gtirb.Section(name=".text", module=m)
        sym = gtirb.Symbol(name="hello", module=m)
        expr = gtirb.SymAddrConst(0, sym)
        bi = gtirb.ByteInterval(symbolic_expressions={1: expr})

        self.assertEqual(set(m.symbolic_expressions_referencing(sym)), set())

        bi.section = s
        self.assertEqual(
            set(m.symbolic_expressions_referencing(sym)), {(bi, 1, expr)}
        )

        bi.section = None
        self.assertEqual(set(m.symbolic_expressions_referencing(sym)), set())

    def test_add_remove_symbolic_expressions(self):
        m = gtirb.Module(name="test")
        s = gtirb.Section(name=".text", module=m)
        sym = gtirb.Symbol(name="hello", module=m)
        expr = gtirb.SymAddrConst(0, sym)
        bi = gtirb.ByteInterval(section=s)

        self.assertEqual(set(m.symbolic_expressions_referencing(sym)), set())

        bi.symbolic_expressions[1] = expr
        self.assertEqual(
            set(m.symbolic_expressions_referencing(sym)), {(bi, 1, expr)}
        )

        bi.symbolic_expressions[2] = expr
        self.assertEqual(
            set(m.symbolic_expressions_referencing(sym)),
            {(bi, 1, expr), (bi, 2, expr)},
        )

        # Ensure that this is a no-op instead of appearing twice
        bi.symbolic_expressions[2] = expr
        self.assertEqual(
            set(m.symbolic_expressions_referencing(sym)),
            {(bi, 1, expr), (bi, 2, expr)},
        )

        del bi.symbolic_expressions[2]
        self.assertEqual(
            set(m.symbolic_expressions_referencing(sym)), {(bi, 1, expr)}
        )

        del bi.symbolic_expressions[1]
        self.assertEqual(set(m.symbolic_expressions_referencing(sym)), set())

    def test_expr_in_multiple_times(self):
        # A single expression can be in multiple byte intervals or in the same
        # byte interval multiple times (but not different modules, given that
        # the symbols they reference can only belong to one module). Test that
        # we handle it correctly.
        m = gtirb.Module(name="test")
        s = gtirb.Section(name=".text", module=m)
        sym = gtirb.Symbol(name="hello", module=m)
        sym2 = gtirb.Symbol(name="world", module=m)
        expr = gtirb.SymAddrConst(0, sym)
        bi1 = gtirb.ByteInterval(section=s)
        bi2 = gtirb.ByteInterval(section=s)

        self.assertEqual(set(m.symbolic_expressions_referencing(sym)), set())

        bi1.symbolic_expressions[1] = expr
        self.assertEqual(
            set(m.symbolic_expressions_referencing(sym)), {(bi1, 1, expr)}
        )

        bi1.symbolic_expressions[2] = expr
        self.assertEqual(
            set(m.symbolic_expressions_referencing(sym)),
            {(bi1, 1, expr), (bi1, 2, expr)},
        )

        bi2.symbolic_expressions[4] = expr
        self.assertEqual(
            set(m.symbolic_expressions_referencing(sym)),
            {(bi1, 1, expr), (bi1, 2, expr), (bi2, 4, expr)},
        )

        # Now try changing the symbolic expression to make sure we update the
        # index.
        expr.symbol = sym2
        self.assertEqual(set(m.symbolic_expressions_referencing(sym)), set())

        self.assertEqual(
            set(m.symbolic_expressions_referencing(sym2)),
            {(bi1, 1, expr), (bi1, 2, expr), (bi2, 4, expr)},
        )
