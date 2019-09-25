import unittest
import gtirb.serialization
import io


class TestSerialization(unittest.TestCase):
    def test_parse_type(self):
        def test_positive(type_name, oracle):
            self.assertEqual(
                gtirb.serialization.Serialization._parse_type(type_name),
                oracle,
            )
        positive_tests = [
            ("mapping", ("mapping", ())),
            ("mapping<FOO,BAR>", ("mapping", (("FOO", ()), ("BAR", ())))),
            (
                "mapping<FOO,set<BAR>>",
                ("mapping", (("FOO", ()), ("set", (("BAR", ()),)))),
            ),
            (
                "mapping<FOO,mapping<BAR,BAZ>>",
                (
                    "mapping",
                    (("FOO", ()), ("mapping", (("BAR", ()), ("BAZ", ())))),
                ),
            ),
            (
                "mapping<mapping<BAR,BAZ>,FOO>",
                (
                    "mapping",
                    (("mapping", (("BAR", ()), ("BAZ", ()))), ("FOO", ())),
                ),
            ),
        ]
        for type_name, oracle in positive_tests:
            test_positive(type_name, oracle)

        def test_negative(type_name):
            with self.assertRaises(
                gtirb.serialization.TypeNameError,
                msg=type_name,
            ):
                gtirb.serialization.Serialization._parse_type(type_name)

        negative_tests = [
            "mapping<<>",
            "mapping<>>",
            "mapping<><>",
            "mapping<<><>>",
            "mapping<<foo><bar>>",
            "mapping<,>",
            "mapping<FOO,>",
            "mapping<,BAR>",
        ]
        for type_name in negative_tests:
            test_negative(type_name)

    def test_unknown_codec(self):
        serializer = gtirb.serialization.Serialization()
        blob = serializer.decode(b"abcd", "foobar")
        self.assertIsInstance(blob, gtirb.serialization.UnknownData)
        self.assertEqual(blob, b"abcd")
        ostream = io.BytesIO()
        serializer.encode(ostream, blob, "foobar")
        self.assertEqual(ostream.getvalue(), b"abcd")

    def test_nested_unknown_codec(self):
        serializer = gtirb.serialization.Serialization()
        ostream = io.BytesIO()
        serializer.encode(ostream, {
            "a": ["b", "c"],
            "d": ["e"],
        }, "mapping<string,sequence<string>>")
        raw_bytes = ostream.getvalue()

        blob = serializer.decode(raw_bytes, "mapping<string,sequence<foobar>>")
        self.assertIsInstance(blob, gtirb.serialization.UnknownData)
        ostream = io.BytesIO()
        serializer.encode(ostream, blob, "mapping<string,sequence<foobar>>")
        self.assertEqual(ostream.getvalue(), raw_bytes)

        blob = serializer.decode(raw_bytes, "mapping<foobar,sequence<string>>")
        self.assertIsInstance(blob, gtirb.serialization.UnknownData)
        ostream = io.BytesIO()
        serializer.encode(ostream, blob, "mapping<foobar,sequence<string>>")
        self.assertEqual(ostream.getvalue(), raw_bytes)


if __name__ == "__main__":
    unittest.main()
