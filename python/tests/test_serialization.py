import unittest
from serialization import Serialization


class TestSerialization(unittest.TestCase):
    def test_get_subtypes(self):
        def test_one(type_name, oracle):
            self.assertEqual(Serialization.get_subtypes(type_name), oracle)
        tests = [
            ('mapping', ('mapping', [])),
            ('mapping<FOO,BAR>',
             ('mapping', ['FOO', 'BAR'])),
            ('mapping<FOO,set<BAR>>',
             ('mapping', ['FOO', 'set<BAR>'])),
            ('mapping<FOO,mapping<BAR,BAZ>>',
             ('mapping', ['FOO', 'mapping<BAR,BAZ>'])),
            ('mapping<mapping<BAR,BAZ>,FOO>',
             ('mapping', ['mapping<BAR,BAZ>', 'FOO'])),
        ]
        for type_name, oracle in tests:
            test_one(type_name, oracle)


if __name__ == '__main__':
    unittest.main()
