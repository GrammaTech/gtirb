import unittest

import gtirb


class ModuleTests(unittest.TestCase):
    def test_lookup_symbol(self):
        m = gtirb.Module(name="test")
        s1 = gtirb.Symbol(name="hello", module=m)
        s2 = gtirb.Symbol(name="world", module=m)

        found = set(m.lookup_symbol("hello"))
        self.assertEqual(found, {s1})

        found = set(m.lookup_symbol("world"))
        self.assertEqual(found, {s2})

        # Change the name to verify we update the index
        s1.name = "world"
        found = set(m.lookup_symbol("hello"))
        self.assertEqual(found, set())

        found = set(m.lookup_symbol("world"))
        self.assertEqual(found, {s1, s2})

        # Discard the symbol to verify we update the index
        m.symbols.discard(s1)
        found = set(m.lookup_symbol("world"))
        self.assertEqual(found, {s2})

        # Now add it back to verify we update the index
        m.symbols.add(s1)
        found = set(m.lookup_symbol("world"))
        self.assertEqual(found, {s1, s2})
