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
            byte_intervals=[gtirb.ByteInterval(address=4, size=4)]
        )
        s2 = gtirb.Section(
            byte_intervals=[gtirb.ByteInterval(address=8, size=8)]
        )
        s3 = gtirb.Section(
            byte_intervals=[gtirb.ByteInterval(address=100, size=1)]
        )
        s4 = gtirb.Section(byte_intervals=[gtirb.ByteInterval(size=1000)])
        m = gtirb.Module(sections=[s1, s2, s3, s4])

        self.assertEquals(m.section_at(3), None)
        self.assertEquals(m.section_at(4), s1)
        self.assertEquals(m.section_at(7), s1)
        self.assertEquals(m.section_at(8), s2)
        self.assertEquals(m.section_at(15), s2)
        self.assertEquals(m.section_at(16), None)
        self.assertEquals(m.section_at(99), None)
        self.assertEquals(m.section_at(100), s3)
        self.assertEquals(m.section_at(101), None)

        self.assertEquals(set(m.sections_in(0, 100)), {s1, s2})
        self.assertEquals(set(m.sections_in(0, 101)), {s1, s2, s3})
        self.assertEquals(set(m.sections_in(0, 102)), {s1, s2, s3})
        self.assertEquals(set(m.sections_in(7, 4)), {s1, s2})
        self.assertEquals(set(m.sections_in(8, 4)), {s2})
        self.assertEquals(set(m.sections_in(17, 80)), set())
