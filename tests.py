#! /usr/bin/env python


import unittest

from red4emacs import sort_arguments

class TestRed4Emacs(unittest.TestCase):

    def setUp(self):
        pass

    def test_nominal(self):
        txt = "def foo(bar): pass"
        out = sort_arguments(txt=txt)
        self.assertTrue(out in txt)

    def test_kwargs(self):
        txt = "def rst(**kwargs, first): pass"
        out = sort_arguments(txt=txt)
        self.assertEqual(out, "def rst(first, **kwargs):")

    def test_order(self):
        txt = "def foo(**kwargs, key=val): pass"
        out = sort_arguments(txt=txt)
        self.assertEqual(out, "def foo(key=val, **kwargs):")

        txt = "def foo(**kwargs, key=val, first): pass"
        correct = "def foo(first, key=val, **kwargs):"
        self.assertEqual(correct, sort_arguments(txt=txt))

    def test_append_last(self):
        txt = "def rst(one, two): pass"
        correct = "def rst(one, two):"
        self.assertEqual(correct, sort_arguments(txt=txt))

    def test_append_kw(self):
        txt = "def rst(one, two=None, three=['inlist']): pass"
        correct = "def rst(one, two=None, three=['inlist']):"
        self.assertEqual(correct, sort_arguments(txt=txt))

    def test_insert_last(self):
        txt = "def foo(self, key=[], second): pass"
        correct = "def foo(self, second, key=[]):"
        self.assertEqual(correct, sort_arguments(txt=txt))

    def test_simple_to_all(self):
        txt = "def foo(self, key=[], second): pass"
        correct = "def foo(self, second, key=[]):"
        self.assertEqual(correct, sort_arguments(txt=txt))

    def test_arg(self):
        txt = "def foo(self, *arg, one): pass"
        correct = "def foo(self, one, *arg):"
        self.assertEqual(correct, sort_arguments(txt=txt))

    def test_arg_kwargs(self):
        txt = "def foo(self, **kwargs, *arg): pass"
        correct = "def foo(self, *arg, **kwargs):"
        self.assertEqual(correct, sort_arguments(txt=txt))

    def test_arg_kwargs_all(self):
        txt = "def foo(self, **kwargs, one, two=[], *arg): pass"
        correct = "def foo(self, one, two=[], *arg, **kwargs):"
        self.assertEqual(correct, sort_arguments(txt=txt))

    def test_kwargs_into_arg(self):
        txt = "def foo(self, *args, **kwargs): pass"
        correct = "def foo(self, *args, **kwargs):"
        self.assertEqual(correct, sort_arguments(txt=txt))

if __name__ == "__main__":
    unittest.main()
