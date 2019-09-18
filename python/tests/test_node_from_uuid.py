import unittest
import gtirb
import uuid


class NodeFromUUIDTest(unittest.TestCase):
    def test_from_uuid(self):
        for node1 in (
            gtirb.IR(),
            gtirb.Module(),
            gtirb.Block(0, 0),
            gtirb.DataObject(0, 0),
            gtirb.ProxyBlock(),
            gtirb.ImageByteMap(),
            gtirb.Section("test", 0, 0),
            gtirb.Symbol("test"),
        ):
            with self.subTest(type(node1).__name__):
                node2 = gtirb.Node.from_uuid(node1.uuid)
                self.assertEqual(node1, node2)

    def test_from_uuid_failed(self):
        bad_id = uuid.uuid4()
        node = gtirb.Node.from_uuid(bad_id)
        self.assertIsNone(node)


if __name__ == "__main__":
    unittest.main()
