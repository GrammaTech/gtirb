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

    def test_from_uuid_typed(self):
        nodes = (
            gtirb.IR(),
            gtirb.Module(),
            gtirb.Block(0, 0),
            gtirb.DataObject(0, 0),
            gtirb.ProxyBlock(),
            gtirb.ImageByteMap(),
            gtirb.Section("test", 0, 0),
            gtirb.Symbol("test"),
        )
        for node1 in nodes:
            with self.subTest(type(node1).__name__):
                for node_cls in map(type, nodes):
                    node2 = None
                    try:
                        node2 = node_cls.from_uuid(node1.uuid)
                    except TypeError:
                        if node_cls == type(node1):
                            self.fail(
                                "%s.from_uuid raised error returning node of"
                                "its own type" % node_cls
                            )
                    else:
                        if node_cls != type(node1):
                            self.fail(
                                "%s.from_uuid returned despite getting"
                                "argument for a node of type %s"
                                % (node_cls, type(node1))
                            )
                        else:
                            self.assertEqual(node1, node2)


if __name__ == "__main__":
    unittest.main()
