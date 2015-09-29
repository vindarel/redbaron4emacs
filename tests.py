#! /usr/bin/env python


import unittest

from red4emacs import main

class TestRed4Emacs(unittest.TestCase):

    def setUp(self):
        pass

    def test_nominal(self):
        txt = "def foo(bar): pass"
        out = main(txt=txt)
        self.assertTrue(out in txt)

    def test_order(self):
        txt = "def foo(**kwargs, first): pass"
        out = main(txt=txt)
        self.assertEqual(out, "def foo(first, **kwargs):")

        txt = "def foo(**kwargs, key=val): pass"
        out = main(txt=txt)
        self.assertEqual(out, "def foo(key=val, **kwargs):")

        txt = "def foo(**kwargs, key=val, first): pass"
        correct = "def foo(first, key=val, **kwargs):"
        self.assertEqual(correct, main(txt=txt))

    def test_insert_last(self):
        txt = "def foo(first, key=val, second): pass"
        correct = "def foo(first, second, key=val):"
        self.assertEqual(correct, main(txt=txt))

if __name__ == "__main__":
    unittest.main()
