import io
import unittest

import gtirb.serialization
from gtirb.serialization import Variant


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
                    name="M",
                    sections=[
                        gtirb.Section(
                            byte_intervals=[
                                gtirb.ByteInterval(blocks=[gtirb.CodeBlock()])
                            ]
                        )
                    ],
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
                    name="M",
                    sections=[
                        gtirb.Section(
                            byte_intervals=[
                                gtirb.ByteInterval(
                                    blocks=[gtirb.CodeBlock(uuid=b.uuid)]
                                )
                            ]
                        )
                    ],
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

    def test_variant_codec(self):
        serializer = gtirb.serialization.Serialization()
        ostream = io.BytesIO()
        variant = Variant(2, {5: ["a", "b"], 15: ["cc", "ddd"]})
        serializer.encode(
            ostream,
            variant,
            "variant<string,int64_t,mapping<int64_t,sequence<string>>>",
        )
        raw_bytes = ostream.getvalue()
        var_val = serializer.decode(
            raw_bytes,
            "variant<string,int64_t,mapping<int64_t,sequence<string>>>",
        )
        self.assertEqual(var_val, variant)

        ostream = io.BytesIO()
        variant = Variant(1, 10)
        serializer.encode(
            ostream, variant, "variant<string,int64_t,string>",
        )
        raw_bytes = ostream.getvalue()
        var_val = serializer.decode(
            raw_bytes, "variant<string,int64_t,string>"
        )
        self.assertEqual(var_val, variant)

        ostream = io.BytesIO()
        variant = Variant(0, "zzzz")
        serializer.encode(
            ostream, variant, "variant<string,int64_t,string>",
        )
        raw_bytes = ostream.getvalue()
        var_val = serializer.decode(
            raw_bytes, "variant<string,int64_t,string>"
        )
        self.assertEqual(var_val, variant)

        ostream = io.BytesIO()
        mapping = {"aa": Variant(0, 5), "bbb": Variant(1, "ccccc")}
        serializer.encode(
            ostream, mapping, "mapping<string,variant<int64_t,string>>",
        )
        raw_bytes = ostream.getvalue()
        mapping_val = serializer.decode(
            raw_bytes, "mapping<string,variant<int64_t,string>>"
        )
        self.assertEqual(mapping_val, mapping)

    def test_int_serializers(self):
        def _check_val(typename, val):
            bstream = io.BytesIO()
            gtirb.AuxData.serializer.encode(bstream, val, typename)
            result = gtirb.AuxData.serializer.decode(
                bstream.getvalue(), typename
            )
            self.assertEqual(result, val)

        # Signed types
        def _test_range(typename, minval, maxval):
            _check_val(typename, 127)
            _check_val(typename, minval)
            _check_val(typename, maxval)

        _test_range("int8_t", -(2 ** 7), (2 ** 7) - 1)
        _test_range("int16_t", -(2 ** 15), (2 ** 15) - 1)
        _test_range("int32_t", -(2 ** 31), (2 ** 31) - 1)
        _test_range("int64_t", -(2 ** 63), (2 ** 63) - 1)
        _test_range("uint8_t", 0, (2 ** 8) - 1)
        _test_range("uint16_t", 0, (2 ** 16) - 1)
        _test_range("uint32_t", 0, (2 ** 32) - 1)
        _test_range("uint64_t", 0, (2 ** 64) - 1)


if __name__ == "__main__":
    unittest.main()
