import unittest
import uuid

import gtirb


class NodeFromUUIDTest(unittest.TestCase):
    def test_get_by_uuid_failed(self):
        bad_id = uuid.uuid4()
        node = gtirb.IR().get_by_uuid(bad_id)
        self.assertIsNone(node)

    def test_get_by_uuid(self):
        ir1 = gtirb.IR()
        ir2 = gtirb.IR()

        # test nodes from one IR don't pollute the cache of other IRs
        self.assertIsNone(ir1.get_by_uuid(ir2.uuid))
        self.assertIsNone(ir2.get_by_uuid(ir1.uuid))
        self.assertEqual(ir1.get_by_uuid(ir1.uuid), ir1)
        self.assertEqual(ir2.get_by_uuid(ir2.uuid), ir2)

        m = gtirb.Module()
        m.ir = ir1
        s = gtirb.Section()
        s.module = m
        bi = gtirb.ByteInterval()
        bi.section = s
        b = gtirb.CodeBlock()
        b.byte_interval = bi
        sym = gtirb.Symbol("test")
        sym.module = m

        # test all nodes are lookupable by any other node type
        self.assertEqual(ir1.get_by_uuid(m.uuid), m)
        self.assertEqual(ir1.get_by_uuid(s.uuid), s)
        self.assertEqual(ir1.get_by_uuid(bi.uuid), bi)
        self.assertEqual(ir1.get_by_uuid(b.uuid), b)
        self.assertEqual(ir1.get_by_uuid(sym.uuid), sym)

        self.assertEqual(m.ir.get_by_uuid(m.uuid), m)
        self.assertEqual(m.ir.get_by_uuid(s.uuid), s)
        self.assertEqual(m.ir.get_by_uuid(bi.uuid), bi)
        self.assertEqual(m.ir.get_by_uuid(b.uuid), b)
        self.assertEqual(m.ir.get_by_uuid(sym.uuid), sym)

        self.assertEqual(s.ir.get_by_uuid(m.uuid), m)
        self.assertEqual(s.ir.get_by_uuid(s.uuid), s)
        self.assertEqual(s.ir.get_by_uuid(bi.uuid), bi)
        self.assertEqual(s.ir.get_by_uuid(b.uuid), b)
        self.assertEqual(s.ir.get_by_uuid(sym.uuid), sym)

        self.assertEqual(bi.ir.get_by_uuid(m.uuid), m)
        self.assertEqual(bi.ir.get_by_uuid(s.uuid), s)
        self.assertEqual(bi.ir.get_by_uuid(bi.uuid), bi)
        self.assertEqual(bi.ir.get_by_uuid(b.uuid), b)
        self.assertEqual(bi.ir.get_by_uuid(sym.uuid), sym)

        self.assertEqual(b.ir.get_by_uuid(m.uuid), m)
        self.assertEqual(b.ir.get_by_uuid(s.uuid), s)
        self.assertEqual(b.ir.get_by_uuid(bi.uuid), bi)
        self.assertEqual(b.ir.get_by_uuid(b.uuid), b)
        self.assertEqual(b.ir.get_by_uuid(sym.uuid), sym)

        self.assertEqual(sym.ir.get_by_uuid(m.uuid), m)
        self.assertEqual(sym.ir.get_by_uuid(s.uuid), s)
        self.assertEqual(sym.ir.get_by_uuid(bi.uuid), bi)
        self.assertEqual(sym.ir.get_by_uuid(b.uuid), b)
        self.assertEqual(sym.ir.get_by_uuid(sym.uuid), sym)

        # test removing a node removes all children as well
        bi.section = None

        self.assertEqual(ir1.get_by_uuid(m.uuid), m)
        self.assertEqual(ir1.get_by_uuid(s.uuid), s)
        self.assertIsNone(ir1.get_by_uuid(bi.uuid))
        self.assertIsNone(ir1.get_by_uuid(b.uuid))
        self.assertEqual(ir1.get_by_uuid(sym.uuid), sym)

        bi.section = s

        self.assertEqual(ir1.get_by_uuid(m.uuid), m)
        self.assertEqual(ir1.get_by_uuid(s.uuid), s)
        self.assertEqual(ir1.get_by_uuid(bi.uuid), bi)
        self.assertEqual(ir1.get_by_uuid(b.uuid), b)
        self.assertEqual(ir1.get_by_uuid(sym.uuid), sym)


if __name__ == "__main__":
    unittest.main()
