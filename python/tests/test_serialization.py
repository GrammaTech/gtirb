import io
import unittest

import gtirb.serialization


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
                gtirb.serialization.TypeNameError, msg=type_name
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
        serializer.encode(
            ostream,
            {"a": ["b", "c"], "d": ["e"]},
            "mapping<string,sequence<string>>",
        )
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

    def test_uuid_codec(self):
        ir = gtirb.IR(
            modules=[
                gtirb.Module(
                    sections=[
                        gtirb.Section(
                            byte_intervals=[
                                gtirb.ByteInterval(blocks=[gtirb.CodeBlock()])
                            ]
                        )
                    ]
                )
            ]
        )
        b = next(iter(ir.code_blocks))

        # test finding block in same IR
        bstream = io.BytesIO()
        gtirb.AuxData.serializer.encode(bstream, b.uuid, "UUID")
        result = gtirb.AuxData.serializer.decode(
            bstream.getvalue(), "UUID", ir.get_by_uuid
        )
        self.assertEqual(result, b)

        # test not finding block with same UUID in other IR
        ir2 = gtirb.IR(
            modules=[
                gtirb.Module(
                    sections=[
                        gtirb.Section(
                            byte_intervals=[
                                gtirb.ByteInterval(
                                    blocks=[gtirb.CodeBlock(uuid=b.uuid)]
                                )
                            ]
                        )
                    ]
                )
            ]
        )
        b2 = next(iter(ir2.code_blocks))

        bstream = io.BytesIO()
        gtirb.AuxData.serializer.encode(bstream, b.uuid, "UUID")
        result = gtirb.AuxData.serializer.decode(
            bstream.getvalue(), "UUID", ir.get_by_uuid
        )
        self.assertEqual(result, b)

        bstream = io.BytesIO()
        gtirb.AuxData.serializer.encode(bstream, b.uuid, "UUID")
        result = gtirb.AuxData.serializer.decode(
            bstream.getvalue(), "UUID", ir2.get_by_uuid
        )
        self.assertEqual(result, b2)

        # test not finding the UUID of a free block
        b3 = gtirb.CodeBlock()

        bstream = io.BytesIO()
        gtirb.AuxData.serializer.encode(bstream, b3.uuid, "UUID")
        result = gtirb.AuxData.serializer.decode(
            bstream.getvalue(), "UUID", ir.get_by_uuid
        )
        self.assertEqual(result, b3.uuid)


if __name__ == "__main__":
    unittest.main()
