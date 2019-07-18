import unittest
from gtirb.imagebytemap import ImageByteMap


byte_map = {10: b'aaaaa', 15: b'bbbbbbb', 110: b'cccccc', 310: b'ffffff'}
ibm = ImageByteMap(addr_min=10,
                   addr_max=315,
                   base_address=10,
                   byte_map=byte_map,
                   entry_point_address=10,
                   uuid=None,
                   uuid_cache={})


def decode_byte(byte):
    return int.from_bytes(byte, byteorder='big')


class TestImageByteMap(unittest.TestCase):
    def test_initialization_coalescing(self):
        self.assertTrue(len(ibm._byte_map) == 3)

    def test_initialization_overlapping_ranges(self):
        bad_byte_map = \
            {10: b'aaaaa', 11: b'bbbbbbb', 110: b'bbbbbb', 310: b'ffffff'}
        with self.assertRaises(ValueError, msg="Overlapping ranges uncaught"):
            ImageByteMap(addr_min=10,
                         addr_max=315,
                         base_address=10,
                         byte_map=bad_byte_map,
                         entry_point_address=10,
                         uuid=None,
                         uuid_cache={})

    def test_contains(self):
        self.assertTrue(10 in ibm)
        self.assertTrue(15 in ibm)
        self.assertTrue(21 in ibm)
        self.assertFalse(22 in ibm)
        self.assertFalse(0 in ibm)
        self.assertFalse("test" in ibm)

    def test_getitem(self):
        self.assertEqual(ibm[10], decode_byte(b'a'))
        self.assertEqual(ibm[14], decode_byte(b'a'))
        self.assertEqual(ibm[15], decode_byte(b'b'))
        self.assertEqual(ibm[13:17], b'aabb')
        self.assertEqual(ibm[110:114], b'cccc')

        def index_error(msg, start, stop, skip=None):
            with self.assertRaises(IndexError, msg=msg):
                ibm[start:stop:skip]

        bad_slices = (
                (0, 15, "start not in map"),
                (10, 50, "stop not in map"),
                (15, 10, "reverse slicing"),
                (15, 310, "gap in bytes"),
        )
        for start, stop, msg in bad_slices:
            index_error(start=start, stop=stop, msg=msg)
        with self.assertRaises(IndexError, msg="slicing unsupported"):
            ibm[15:16:2]

        with self.assertRaises(TypeError, msg="no stop"):
            ibm[15:]
        with self.assertRaises(TypeError, msg="no start"):
            ibm[:15]
        with self.assertRaises(TypeError, msg="bad index"):
            ibm["test"]

    def test_iter(self):
        simple_byte_map = \
            {0: b'aa', 10: b'bb', 12: b'cc'}
        simple_ibm = ImageByteMap(addr_min=0,
                                  addr_max=15,
                                  base_address=0,
                                  byte_map=simple_byte_map,
                                  entry_point_address=10,
                                  uuid=None,
                                  uuid_cache={})
        self.assertEqual(list(simple_ibm),
                         [(0, decode_byte(b'a')),
                          (1, decode_byte(b'a')),
                          (10, decode_byte(b'b')),
                          (11, decode_byte(b'b')),
                          (12, decode_byte(b'c')),
                          (13, decode_byte(b'c'))])

    def test_len(self):
        self.assertEqual(len(ibm), 24)

    def test_setitem_single(self):
        ibm = ImageByteMap(addr_min=0,
                           addr_max=15,
                           base_address=0,
                           byte_map={},
                           entry_point_address=10,
                           uuid=None,
                           uuid_cache={})
        self.assertEqual(len(ibm), 0)
        ibm[10] = 0
        self.assertEqual(ibm[10], 0)
        self.assertEqual(list(ibm), [(10, 0)])
        self.assertEqual(ibm._start_addresses, [10])
        ibm[11] = 255
        self.assertEqual(ibm[11], 255)
        self.assertEqual(list(ibm), [(10, 0), (11, 255)])
        self.assertEqual(ibm._start_addresses, [10])
        ibm[14] = 0
        self.assertEqual(ibm[14], 0)
        self.assertEqual(list(ibm), [(10, 0), (11, 255), (14, 0)])
        self.assertEqual(ibm._start_addresses, [10, 14])
        ibm[13] = 0
        self.assertEqual(ibm[13], 0)
        self.assertEqual(list(ibm), [(10, 0), (11, 255), (13, 0), (14, 0)])
        self.assertEqual(ibm._start_addresses, [10, 13])
        ibm[12] = 0
        self.assertEqual(ibm[12], 0)
        self.assertEqual(list(ibm), [(10, 0), (11, 255), (12, 0),
                                     (13, 0), (14, 0)])
        self.assertEqual(ibm._start_addresses, [10])
