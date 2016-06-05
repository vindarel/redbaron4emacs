#! /usr/bin/env python

from redbaron import RedBaron
from toolz.itertoolz import interpose


"""First goal: add an argument to a method, not stupidly: sort them,
i.e. put "self" first, put "**kwargs" last, put a named parameter
after a simple one.

Be an interface to emacs functions.

"""
# ideas: rm params (helm choice list), set this method static (rm self) (baron not needed)

def rm_comma(arg):
    return arg.get('type') != 'comma'

def put_commas(args):
    comma = {'first_formatting': [], 'type': 'comma', 'second_formatting': [{'type': 'space', 'value': ' '}]}
    res = interpose(comma, args)
    return list(res)

def arg_type(arg):
    return arg['type']

def arg_lower(x, y):
    """Comparaison function, to sort arguments.

    x, y: arg dicts

    types:
    - def_argument, comma, dict_argument

    but in def_argument, we can have named arguments. That one has
    another dict for value.

    TODO: if "else", then lower !
    """
    xtype = x.get('type')
    ytype = y.get('type')
    X_LT = -1
    X_GT = 1
    EQ = 0
    if xtype == 'def_argument':
        if ytype != 'def_argument':
            return X_LT
        else:
            # Both x and y are def_argument
            # 'self' is always the lower arg.
            if x.get('target') and x.get('target').get('value') == 'self':
                return X_LT
            if y.get('target') and y.get('target').get('value') == 'self':
                return X_GT
            if not y.get('value'):
                # just a simple argument
                return X_GT
            else:
                if not x.get('value'):
                    # x is a simple arg, not a kw
                    return X_LT
            # they're both named args
            return X_GT

    if y.get('type') == 'def_argument':
        return X_GT

    print "we shouldn't get here !"

def reform_input(args, method="foo"):
    """Re-give the def repr.

    - args: a list of dicts, representing redbaron's arguments

    - return: something like "def foo(args):" (without 'pass')
    """
    # https://redbaron.readthedocs.io/en/latest/nodes_reference.html#funcdefnode
    args = put_commas(args)
    newdef = "def {}(): pass".format(method)
    red = RedBaron(newdef)
    red[0].arguments = args
    res = red.dumps().strip()
    res = res.strip(" pass")
    return res

def sort_arguments(txt=""):
    """be careful, txt must be valid python code.

    example:
    : def foo(arg): pass
    """

    if not txt:
        txt = "def foo(first, **kwargs, second=foo): pass"
    # print "orig txt: \n", txt
    red = RedBaron(txt)

    fst = red.fst()[0]
    args = fst['arguments']
    # print "orig: ", args
    # print map(arg_type, args)
    # print map(rm_comma, args)
    args = filter(rm_comma, args)
    sargs = sorted(args, cmp=arg_lower)
    # print args
    res = reform_input(sargs, method=fst['name'])
    return res

if __name__ == "__main__":
    import sys
    txt = "def foo(self, key=val, second): pass" # testing
    if len(sys.argv) > 1:
        res = sort_arguments(txt=sys.argv[1])
        print res

    else:
        exit(sort_arguments(txt=txt))
