import unittest

import gtirb.util


class RecordingList(gtirb.util.ListWrapper):
    def __init__(self, *args):
        self.record = []
        super().__init__(*args)

    def _add(self, value):
        self.record.append(("add", value))

    def _remove(self, value):
        self.record.append(("remove", value))


class ListWrapperTests(unittest.TestCase):
    def test_empty_init(self):
        lst = RecordingList()
        self.assertFalse(lst.record)

    def test_values_init(self):
        lst = RecordingList(["A", "B", "C"])
        self.assertEqual(
            lst.record, [("add", "A"), ("add", "B"), ("add", "C")]
        )

    def test_getitem_int(self):
        lst = RecordingList(["A", "B", "C"])
        self.assertEqual(lst[0], "A")
        self.assertEqual(lst[1], "B")
        self.assertEqual(lst[2], "C")

    def test_getitem_slice(self):
        lst = RecordingList(["A", "B", "C"])
        self.assertEqual(lst[1:2], ["B"])
        self.assertEqual(lst[:2], ["A", "B"])
        self.assertEqual(lst[1:], ["B", "C"])

    def test_setitem_int(self):
        lst = RecordingList(["A", "B", "C"])
        lst[1] = "D"
        self.assertEqual(list(lst), ["A", "D", "C"])
        self.assertEqual(
            lst.record,
            [
                ("add", "A"),
                ("add", "B"),
                ("add", "C"),
                ("remove", "B"),
                ("add", "D"),
            ],
        )

    def test_setitem_slice(self):
        lst = RecordingList(["A", "B", "C"])
        lst[1:2] = ["D", "E"]
        self.assertEqual(list(lst), ["A", "D", "E", "C"])
        self.assertEqual(
            lst.record,
            [
                ("add", "A"),
                ("add", "B"),
                ("add", "C"),
                ("remove", "B"),
                ("add", "D"),
                ("add", "E"),
            ],
        )

    def test_delitem_int(self):
        lst = RecordingList(["A", "B"])
        del lst[0]
        self.assertEqual(list(lst), ["B"])
        self.assertEqual(
            lst.record, [("add", "A"), ("add", "B"), ("remove", "A")]
        )

    def test_delitem_slice(self):
        lst = RecordingList(["A", "B", "C"])
        del lst[1:]
        self.assertEqual(list(lst), ["A"])
        self.assertEqual(
            lst.record,
            [
                ("add", "A"),
                ("add", "B"),
                ("add", "C"),
                ("remove", "B"),
                ("remove", "C"),
            ],
        )

    def test_len(self):
        lst = RecordingList(["A", "B", "C"])
        self.assertEqual(len(lst), 3)

    def test_insert(self):
        lst = RecordingList(["A", "B"])
        lst.insert(1, "C")
        self.assertEqual(list(lst), ["A", "C", "B"])
        self.assertEqual(
            lst.record, [("add", "A"), ("add", "B"), ("add", "C")]
        )

    def test_append(self):
        lst = RecordingList(["A", "B"])
        lst.append("C")
        self.assertEqual(list(lst), ["A", "B", "C"])
        self.assertEqual(
            lst.record, [("add", "A"), ("add", "B"), ("add", "C")]
        )

    def test_remove(self):
        lst = RecordingList(["A", "B"])
        lst.remove("A")
        self.assertEqual(list(lst), ["B"])
        self.assertEqual(
            lst.record, [("add", "A"), ("add", "B"), ("remove", "A")]
        )

    def test_extend(self):
        lst = RecordingList()
        lst.extend(["A", "B"])
        self.assertEqual(list(lst), ["A", "B"])
        self.assertEqual(lst.record, [("add", "A"), ("add", "B")])

    def test_str(self):
        lst = RecordingList(["A", "B"])
        self.assertEqual(str(lst), str(["A", "B"]))
