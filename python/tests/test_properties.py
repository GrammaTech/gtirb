import unittest

import gtirb


class TestProperties(unittest.TestCase):
    def test_data_blocks(self):
        b = gtirb.DataBlock()

        self.assertEquals(b.address, None)
        self.assertEquals(b.contents, b"")
        self.assertEquals(set(b.references), set())

        bi = gtirb.ByteInterval(address=1, contents=b"abcd1234")
        b.offset = 2
        b.size = 3
        b.byte_interval = bi

        self.assertEquals(b.address, 3)
        self.assertEquals(b.contents, b"cd1")
        self.assertEquals(set(b.references), set())

        s = gtirb.Section()
        bi.section = s
        m = gtirb.Module()
        sym1 = gtirb.Symbol("test", payload=b)
        sym2 = gtirb.Symbol("test", payload=123)
        sym3 = gtirb.Symbol("test", payload=b)
        m.symbols |= {sym1, sym2, sym3}
        s.module = m

        self.assertEquals(b.address, 3)
        self.assertEquals(b.contents, b"cd1")
        self.assertEquals(set(b.references), {sym1, sym3})

    def test_code_blocks(self):
        b = gtirb.CodeBlock()

        self.assertEquals(b.address, None)
        self.assertEquals(b.contents, b"")
        self.assertEquals(set(b.references), set())
        self.assertEquals(set(b.incoming_edges), set())
        self.assertEquals(set(b.outgoing_edges), set())

        bi = gtirb.ByteInterval(address=1, contents=b"abcd1234")
        b.offset = 2
        b.size = 3
        b.byte_interval = bi

        self.assertEquals(b.address, 3)
        self.assertEquals(b.contents, b"cd1")
        self.assertEquals(set(b.references), set())
        self.assertEquals(set(b.incoming_edges), set())
        self.assertEquals(set(b.outgoing_edges), set())

        s = gtirb.Section()
        bi.section = s
        m = gtirb.Module()
        sym1 = gtirb.Symbol("test", payload=b)
        sym2 = gtirb.Symbol("test", payload=123)
        sym3 = gtirb.Symbol("test", payload=b)
        m.symbols |= {sym1, sym2, sym3}
        s.module = m

        self.assertEquals(b.address, 3)
        self.assertEquals(b.contents, b"cd1")
        self.assertEquals(set(b.references), {sym1, sym3})
        self.assertEquals(set(b.incoming_edges), set())
        self.assertEquals(set(b.outgoing_edges), set())

        i = gtirb.IR()
        m.ir = i
        e1 = gtirb.Edge(b, gtirb.ProxyBlock())
        e2 = gtirb.Edge(gtirb.ProxyBlock(), b)
        e3 = gtirb.Edge(gtirb.ProxyBlock(), gtirb.ProxyBlock())
        e4 = gtirb.Edge(b, b)
        i.cfg |= {e1, e2, e3, e4}

        self.assertEquals(b.address, 3)
        self.assertEquals(b.contents, b"cd1")
        self.assertEquals(set(b.references), {sym1, sym3})
        self.assertEquals(set(b.incoming_edges), {e2, e4})
        self.assertEquals(set(b.outgoing_edges), {e1, e4})

    def test_proxy_blocks(self):
        b = gtirb.ProxyBlock()

        self.assertEquals(set(b.references), set())
        self.assertEquals(set(b.incoming_edges), set())
        self.assertEquals(set(b.outgoing_edges), set())

        m = gtirb.Module()
        sym1 = gtirb.Symbol("test", payload=b)
        sym2 = gtirb.Symbol("test", payload=123)
        sym3 = gtirb.Symbol("test", payload=b)
        m.symbols |= {sym1, sym2, sym3}
        b.module = m

        self.assertEquals(set(b.references), {sym1, sym3})
        self.assertEquals(set(b.incoming_edges), set())
        self.assertEquals(set(b.outgoing_edges), set())

        i = gtirb.IR()
        m.ir = i
        e1 = gtirb.Edge(b, gtirb.ProxyBlock())
        e2 = gtirb.Edge(gtirb.ProxyBlock(), b)
        e3 = gtirb.Edge(gtirb.ProxyBlock(), gtirb.ProxyBlock())
        e4 = gtirb.Edge(b, b)
        i.cfg |= {e1, e2, e3, e4}

        self.assertEquals(set(b.references), {sym1, sym3})
        self.assertEquals(set(b.incoming_edges), {e2, e4})
        self.assertEquals(set(b.outgoing_edges), {e1, e4})

    def test_sections(self):
        s = gtirb.Section()
        self.assertEquals(s.address, None)
        self.assertEquals(s.size, None)

        s.byte_intervals.clear()
        s.byte_intervals |= {gtirb.ByteInterval()}
        self.assertEquals(s.address, None)
        self.assertEquals(s.size, None)

        s.byte_intervals.clear()
        s.byte_intervals |= {gtirb.ByteInterval(size=3)}
        self.assertEquals(s.address, None)
        self.assertEquals(s.size, None)

        s.byte_intervals.clear()
        s.byte_intervals |= {gtirb.ByteInterval(address=2, size=4)}
        self.assertEquals(s.address, 2)
        self.assertEquals(s.size, 4)

        s.byte_intervals.clear()
        s.byte_intervals |= {
            gtirb.ByteInterval(address=2, size=4),
            gtirb.ByteInterval(size=3),
        }
        self.assertEquals(s.address, None)
        self.assertEquals(s.size, None)

        s.byte_intervals.clear()
        s.byte_intervals |= {
            gtirb.ByteInterval(address=2, size=4),
            gtirb.ByteInterval(address=100, size=3),
        }
        self.assertEquals(s.address, 2)
        self.assertEquals(s.size, 101)

    def test_modules(self):
        s1 = gtirb.Section(
            name="s1", byte_intervals=[gtirb.ByteInterval(address=4, size=4)]
        )
        s2 = gtirb.Section(
            name="s2", byte_intervals=[gtirb.ByteInterval(address=8, size=8)]
        )
        s3 = gtirb.Section(
            name="s3", byte_intervals=[gtirb.ByteInterval(address=100, size=1)]
        )
        s4 = gtirb.Section(
            name="s4", byte_intervals=[gtirb.ByteInterval(size=1000)]
        )
        m = gtirb.Module(sections=[s1, s2, s3, s4])

        self.assertEquals(set(m.sections_on(3)), set())
        self.assertEquals(set(m.sections_on(4)), {s1})
        self.assertEquals(set(m.sections_on(7)), {s1})
        self.assertEquals(set(m.sections_on(8)), {s2})
        self.assertEquals(set(m.sections_on(15)), {s2})
        self.assertEquals(set(m.sections_on(16)), set())
        self.assertEquals(set(m.sections_on(99)), set())
        self.assertEquals(set(m.sections_on(100)), {s3})
        self.assertEquals(set(m.sections_on(101)), set())

        self.assertEquals(set(m.sections_on(range(0, 100))), {s1, s2})
        self.assertEquals(set(m.sections_on(range(0, 101))), {s1, s2, s3})
        self.assertEquals(set(m.sections_on(range(0, 102))), {s1, s2, s3})
        self.assertEquals(set(m.sections_on(range(7, 7 + 4))), {s1, s2})
        self.assertEquals(set(m.sections_on(range(8, 8 + 4))), {s2})
        self.assertEquals(set(m.sections_on(range(17, 17 + 80))), set())

        self.assertEquals(set(m.sections_at(3)), set())
        self.assertEquals(set(m.sections_at(4)), {s1})
        self.assertEquals(set(m.sections_at(5)), set())
        self.assertEquals(set(m.sections_at(7)), set())
        self.assertEquals(set(m.sections_at(8)), {s2})
        self.assertEquals(set(m.sections_at(9)), set())
        self.assertEquals(set(m.sections_at(99)), set())
        self.assertEquals(set(m.sections_at(100)), {s3})
        self.assertEquals(set(m.sections_at(101)), set())

        self.assertEquals(set(m.sections_at(range(0, 100))), {s1, s2})
        self.assertEquals(set(m.sections_at(range(0, 101))), {s1, s2, s3})
        self.assertEquals(set(m.sections_at(range(5, 10))), {s2})
        self.assertEquals(set(m.sections_at(range(95, 105))), {s3})

    def test_byte_intervals(self):
        s = gtirb.Symbol(name="test")
        se1 = gtirb.SymAddrConst(0, s)
        se2 = gtirb.SymStackConst(0, s)
        se3 = gtirb.SymAddrAddr(0, 1, s, s)
        bi = gtirb.ByteInterval(
            address=10, size=10, symbolic_expressions={0: se1, 4: se2, 9: se3}
        )

        self.assertEqual(set(bi.symbolic_expressions_at(9)), set())
        self.assertEqual(set(bi.symbolic_expressions_at(10)), {(bi, 0, se1)})
        self.assertEqual(set(bi.symbolic_expressions_at(11)), set())
        self.assertEqual(set(bi.symbolic_expressions_at(13)), set())
        self.assertEqual(set(bi.symbolic_expressions_at(14)), {(bi, 4, se2)})
        self.assertEqual(set(bi.symbolic_expressions_at(15)), set())
        self.assertEqual(set(bi.symbolic_expressions_at(18)), set())
        self.assertEqual(set(bi.symbolic_expressions_at(19)), {(bi, 9, se3)})
        self.assertEqual(set(bi.symbolic_expressions_at(20)), set())

        self.assertEqual(set(bi.symbolic_expressions_at(range(0, 9))), set())
        self.assertEqual(set(bi.symbolic_expressions_at(range(11, 14))), set())
        self.assertEqual(set(bi.symbolic_expressions_at(range(20, 90))), set())
        self.assertEqual(
            set(bi.symbolic_expressions_at(range(0, 90))),
            {(bi, 0, se1), (bi, 4, se2), (bi, 9, se3)},
        )
        self.assertEqual(
            set(bi.symbolic_expressions_at(range(10, 20))),
            {(bi, 0, se1), (bi, 4, se2), (bi, 9, se3)},
        )
        self.assertEqual(
            set(bi.symbolic_expressions_at(range(10, 19))),
            {(bi, 0, se1), (bi, 4, se2)},
        )
        self.assertEqual(
            set(bi.symbolic_expressions_at(range(11, 18))), {(bi, 4, se2)}
        )
