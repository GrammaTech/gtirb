import unittest
from gtirb import ImageByteMap


def decode_byte(byte):
    return int.from_bytes(byte, byteorder='big')


class TestImageByteMap(unittest.TestCase):
    def test_initialization_coalescing(self):
        byte_map = {10: b'aaaaa', 15: b'bbbbbbb',
                    110: b'cccccc', 310: b'ffffff'}
        ibm = ImageByteMap(addr_min=10,
                           addr_max=315,
                           base_address=10,
                           byte_map=byte_map,
                           entry_point_address=10,
                           uuid=None)
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
                         uuid=None)

    def test_contains(self):
        byte_map = {10: b'aaaaa', 15: b'bbbbbbb',
                    110: b'cccccc', 310: b'ffffff'}
        ibm = ImageByteMap(addr_min=10,
                           addr_max=315,
                           base_address=10,
                           byte_map=byte_map,
                           entry_point_address=10,
                           uuid=None)
        self.assertTrue(10 in ibm)
        self.assertTrue(15 in ibm)
        self.assertTrue(21 in ibm)
        self.assertFalse(22 in ibm)
        self.assertFalse(0 in ibm)
        self.assertFalse("test" in ibm)

    def test_delitem_single(self):
        byte_map = {5: [0] * 5}
        ibm = ImageByteMap(addr_min=0,
                           addr_max=20,
                           base_address=10,
                           byte_map=byte_map,
                           entry_point_address=10,
                           uuid=None)
        self.assertEqual(list(ibm), [(5, 0), (6, 0), (7, 0), (8, 0), (9, 0)])
        self.assertEqual(ibm._start_addresses, [5])
        del ibm[5]
        self.assertEqual(list(ibm), [(6, 0), (7, 0), (8, 0), (9, 0)])
        self.assertEqual(ibm._start_addresses, [6])
        del ibm[9]
        self.assertEqual(list(ibm), [(6, 0), (7, 0), (8, 0)])
        self.assertEqual(ibm._start_addresses, [6])
        del ibm[7]
        self.assertEqual(list(ibm), [(6, 0), (8, 0)])
        self.assertEqual(ibm._start_addresses, [6, 8])

        with self.assertRaises(IndexError, msg="nonexistent key delete"):
            del ibm[20]

    def test_delitem_slice(self):

        def test_slice(del_slice, expected_list, expected_starts, msg):
            byte_map = {5: [0] * 5}
            ibm = ImageByteMap(addr_min=0,
                               addr_max=20,
                               base_address=10,
                               byte_map=byte_map,
                               entry_point_address=10,
                               uuid=None)
            del ibm[del_slice]
            self.assertEqual(list(ibm), expected_list, msg=msg)
            self.assertEqual(ibm._start_addresses, expected_starts)

        test_slices = (
            (slice(5, 6, None), [(6, 0), (7, 0), (8, 0), (9, 0)], [6]),
            (slice(9, 10, None), [(5, 0), (6, 0), (7, 0), (8, 0)], [5]),
            (slice(5, 9, None), [(9, 0)], [9]),
            (slice(5, 10, None), [], []),
            (slice(6, 10, None), [(5, 0)], [5]),
            (slice(6, 8, None), [(5, 0), (8, 0), (9, 0)], [5, 8]),
        )

        test_index = 1
        for (del_slice, expected_list, expected_starts) in test_slices:
            msg = "Test slice %d" % test_index
            test_slice(del_slice, expected_list, expected_starts, msg)
            test_index += 1

        byte_map = {10: b'aaaaa', 15: b'bbbbbbb',
                    110: b'cccccc', 310: b'ffffff'}
        ibm = ImageByteMap(addr_min=10,
                           addr_max=315,
                           base_address=10,
                           byte_map=byte_map,
                           entry_point_address=10,
                           uuid=None)

        def index_error(test_slice, msg):
            with self.assertRaises(IndexError, msg=msg):
                ibm[test_slice]

        bad_slices = (
            (slice(0, 15, None), "start not in map"),
            (slice(10, 50, None), "stop not in map"),
            (slice(15, 10, None), "reverse slicing"),
            (slice(15, 310, None), "gap in bytes"),
            (slice(15, 16, 2), "slicing unsupported"),
        )
        for (test_slice, msg) in bad_slices:
            index_error(test_slice, msg)

    def test_getitem(self):
        byte_map = {10: b'aaaaa', 15: b'bbbbbbb',
                    110: b'cccccc', 310: b'ffffff'}
        ibm = ImageByteMap(addr_min=10,
                           addr_max=315,
                           base_address=10,
                           byte_map=byte_map,
                           entry_point_address=10,
                           uuid=None)

        self.assertEqual(ibm[10], decode_byte(b'a'))
        self.assertEqual(ibm[14], decode_byte(b'a'))
        self.assertEqual(ibm[15], decode_byte(b'b'))
        self.assertEqual(ibm[13:17], b'aabb')
        self.assertEqual(ibm[110:114], b'cccc')

        def index_error(test_slice, msg):
            with self.assertRaises(IndexError, msg=msg):
                ibm[test_slice]

        bad_slices = (
            (slice(0, 15, None), "start not in map"),
            (slice(10, 50, None), "stop not in map"),
            (slice(15, 10, None), "reverse slicing"),
            (slice(15, 310, None), "gap in bytes"),
            (slice(15, 16, 2), "slicing unsupported"),
        )
        for (test_slice, msg) in bad_slices:
            index_error(test_slice, msg)

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
                                  uuid=None)
        self.assertEqual(list(simple_ibm),
                         [(0, decode_byte(b'a')),
                          (1, decode_byte(b'a')),
                          (10, decode_byte(b'b')),
                          (11, decode_byte(b'b')),
                          (12, decode_byte(b'c')),
                          (13, decode_byte(b'c'))])

    def test_len(self):
        byte_map = {10: b'aaaaa', 15: b'bbbbbbb',
                    110: b'cccccc', 310: b'ffffff'}
        ibm = ImageByteMap(addr_min=10,
                           addr_max=315,
                           base_address=10,
                           byte_map=byte_map,
                           entry_point_address=10,
                           uuid=None)
        self.assertEqual(len(ibm), 24)

    def test_setitem_single(self):
        ibm = ImageByteMap(addr_min=5,
                           addr_max=15,
                           base_address=0,
                           byte_map={},
                           entry_point_address=10,
                           uuid=None)
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

        with self.assertRaises(IndexError, msg="write out of range low"):
            ibm[0] = 0
        with self.assertRaises(IndexError, msg="write out of range high"):
            ibm[20] = 0

    def test_setitem_slice(self):
        ibm = ImageByteMap(addr_min=5,
                           addr_max=15,
                           base_address=0,
                           byte_map={},
                           entry_point_address=10,
                           uuid=None)
        ibm[5:] = [1, 2, 3, 4, 5]
        self.assertEqual(ibm[5], 1)
        self.assertEqual(ibm[9], 5)
        self.assertEqual(len(ibm), 5)
        self.assertEqual(list(ibm), [(5, 1), (6, 2), (7, 3), (8, 4), (9, 5)])

        def index_error(test_slice, test_data, msg):
            with self.assertRaises(IndexError, msg=msg):
                ibm[test_slice] = test_data

        bad_indices = (
            (slice(None, 10, None), b'abc123', "start address required"),
            (slice(5, None, 3), b'abc123', "step size unsupported"),
            (slice(15, 10, None), b'abc123', "start after stop"),
            (slice(15, None, None), b'abc123', "write past maximum address"),
            (slice(0, None, None), b'abc123', "write before minimum address"),
        )
        for (test_slice, test_data, msg) in bad_indices:
            index_error(test_slice, test_data, msg)

        with self.assertRaises(ValueError, msg="slice/data size mismatch"):
            ibm[5:7] = b'abc123'

        with self.assertRaises(TypeError, msg="not an iterable"):
            ibm[6:] = "badstring"
