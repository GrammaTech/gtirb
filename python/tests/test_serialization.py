import unittest
from gtirb.serialization import Serialization, TypeNameError


class TestSerialization(unittest.TestCase):
    def test_parse_type(self):
        def test_positive(type_name, oracle):
            self.assertEqual(Serialization._parse_type(type_name), oracle)
        positive_tests = [
            ('mapping', ('mapping', ())),
            ('mapping<FOO,BAR>',
             ('mapping', (('FOO', ()), ('BAR', ())))),
            ('mapping<FOO,set<BAR>>',
             ('mapping', (('FOO', ()), ('set', (('BAR', ()),))))),
            ('mapping<FOO,mapping<BAR,BAZ>>',
             ('mapping', (('FOO', ()),
                          ('mapping', (('BAR', ()), ('BAZ', ())))))),
            ('mapping<mapping<BAR,BAZ>,FOO>',
             ('mapping', (('mapping', (('BAR', ()), ('BAZ', ()))),
                          ('FOO', ())))),
        ]
        for type_name, oracle in positive_tests:
            test_positive(type_name, oracle)

        def test_negative(type_name):
            with self.assertRaises(TypeNameError, msg=type_name):
                Serialization._parse_type(type_name)

        negative_tests = [
            'mapping<<>',
            'mapping<>>',
            'mapping<><>',
            'mapping<<><>>',
            'mapping<<foo><bar>>',
        ]
        for type_name in negative_tests:
            test_negative(type_name)


if __name__ == '__main__':
    unittest.main()
