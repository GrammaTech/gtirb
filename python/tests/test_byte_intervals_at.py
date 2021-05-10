import unittest

import gtirb
from helpers import SearchScope, parameterize_one


class ByteIntervalsAtTests(unittest.TestCase):
    @parameterize_one(
        "scope", (SearchScope.ir, SearchScope.module, SearchScope.section)
    )
    def test_byte_intervals_at(self, scope):
        ir = gtirb.IR()
        m = gtirb.Module(name="test", ir=ir)
        s = gtirb.Section(module=m)
        search_in = scope.select(ir, m, s, None)

        bi1 = gtirb.ByteInterval(address=0x1000, size=4, section=s)
        bi2 = gtirb.ByteInterval(address=0x1004, size=4, section=s)

        found = set(search_in.byte_intervals_at(0x1000))
        self.assertEqual(found, {bi1})

        found = set(search_in.byte_intervals_at(0x1001))
        self.assertEqual(found, set())

        found = set(search_in.byte_intervals_at(range(0x1000, 0x1008)))
        self.assertEqual(found, {bi1, bi2})

        found = set(search_in.byte_intervals_at(range(0x1000, 0x1008, 16)))
        self.assertEqual(found, {bi1})

        # Change the address to verify we update the index
        bi2.address = 0x2000

        found = set(search_in.byte_intervals_at(0x1004))
        self.assertEqual(found, set())

        found = set(search_in.byte_intervals_at(0x2000))
        self.assertEqual(found, {bi2})

        # Discard the interval to verify we update the index
        bi2.section = None

        found = set(search_in.byte_intervals_at(0x2000))
        self.assertEqual(found, set())

        # Now add it back to verify we update the index
        s.byte_intervals.add(bi2)
        found = set(search_in.byte_intervals_at(0x2000))
        self.assertEqual(found, {bi2})
