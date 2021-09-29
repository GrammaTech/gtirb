import unittest
from unittest import mock

import gtirb


class AuxDataTest(unittest.TestCase):
    def setUp(self):
        self.fake_ir = mock.MagicMock()
        self.fake_ir.get_by_uuid = None

    def test_lazy(self):
        ad1 = gtirb.AuxData("test1", "string")
        self.assertEqual(ad1.data, "test1")

        serialized = ad1._to_protobuf()
        ad2 = gtirb.AuxData._from_protobuf(serialized, self.fake_ir)
        # Peek inside: the data is not yet deserialized
        self.assertTrue(ad2._data is None)

        # Accessing the data should deserialize
        self.assertEqual(ad1.data, ad2.data)
        self.assertTrue(ad2._data is not None)

        # Just exercise repr
        self.assertEqual(
            repr(ad2), "AuxData(type_name='string', data='test1', )"
        )

    def test_lazy_never_deserialized(self):
        serialized = gtirb.AuxData("testing 123", "string")._to_protobuf()

        ad1 = gtirb.AuxData._from_protobuf(serialized, self.fake_ir)
        # Peek inside: the data is not yet deserialized
        self.assertTrue(ad1._data is None)

        serialized2 = ad1._to_protobuf()
        self.assertTrue(ad1._data is None)
        self.assertEqual(serialized, serialized2)


if __name__ == "__main__":
    unittest.main()
