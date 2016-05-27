#! /usr/bin/env python

from redbaron import RedBaron

"""First goal: add an argument to a method, not stupidly: sort them,
i.e. put "self" first, put "**kwargs" last, put a named parameter
after a simple one.

Be an interface to emacs functions.

"""
# ideas: rm params (helm choice list), set this method static (rm self) (baron not needed)

def rm_comma(arg):
    return arg.get('type') != 'comma'

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
            if x.get('target') and x.get('target').get('value') == 'self':
                return X_LT
            if y.get('target') and y.get('target').get('value') == 'self':
                return X_GT
            # x, y are def_argument
            if not x.get('value'):
                # just a simple argument
                return X_LT
            else:
                if not y.get('value'):
                    # x is more than a simple arg, y is simple.
                    return X_GT
            # they're both named args
            return X_LT

    if y.get('type') == 'def_argument':
        return X_GT

    print "we shouldn't get here !"

def print_arg(arg):
    # redbaron really doesn't have a method to re-build the input ?
    if type(arg) == type("string"):
        return arg

    if arg.get('target'):
        res = arg['target']['value']
        if arg.get('value'):
            res += "=" + print_arg(arg['value'])

    elif arg.get('value'):
        prefix = ""
        if arg.get('type') == "dict_argument":
            prefix = "**"
        res = prefix + print_arg(arg.get('value'))
    else:
        print "woops, see again print_arg method."

    return res

def reform_input(args, method="foo"):
    """re-give the string:

    def foo(args):
    """
    res = ", ".join(map(print_arg, args))
    res = "def {}({}):".format(method, res)
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

    args = red.fst()[0]['arguments']
    # print "orig: ", args
    # print map(arg_type, args)
    # print map(rm_comma, args)
    args = filter(rm_comma, args)
    args = sorted(args, cmp=arg_lower)
    # print args
    res = reform_input(args)
    return res

if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1:
        res = sort_arguments(txt=sys.argv[1])
        print res

    else:
        exit(sort_arguments())
