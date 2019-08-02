# encoding: utf-8
from collections import Mapping, MutableMapping
from pprint import pprint
from weakref import ref as wkref

from mo_logs import Log

from pyparsing.utils import PY_3, _generatorType, _ustr, _xml_escape, basestring

ParserElement, Forward, Group = [None]*3

_get = object.__getattribute__;

def get_name(tok):
    try:
        if isinstance(tok, Forward):
            return tok.type_for_result.expr.resultsName
        if isinstance(tok, ParseResults):
            return tok.type_for_result.resultsName
        return None
    except Exception as e:
        raise e

def get_tokens(tok):
    if isinstance(tok, ParseResults):
        return _get(tok, "tokens_for_result")
    return None


class ParseResults(object):
    """Structured parse results, to provide multiple means of access to
    the parsed data:

       - as a list (``len(results)``)
       - by list index (``results[0], results[1]``, etc.)
       - by attribute (``results.<resultsName>`` - see :class:`ParserElement.setResultsName`)

    Example::(pars

        integer = Word(nums)
        date_str = (integer.setResultsName("year") + '/'
                        + integer.setResultsName("month") + '/'
                        + integer.setResultsName("day"))
        # equivalent form:
        # date_str = integer("year") + '/' + integer("month") + '/' + integer("day")

        # parseString returns a ParseResults object
        result = date_str.parseString("1999/12/31")

        def test(s, fn=repr):
            print("%s -> %s" % (s, fn(eval(s))))
        test("list(result)")
        test("result[0]")
        test("result['month']")
        test("result.day")
        test("'month' in result")
        test("'minutes' in result")
        test("result.dump()", str)

    prints::

        list(result) -> ['1999', '/', '12', '/', '31']
        result[0] -> '1999'
        result['month'] -> '12'
        result.day -> '31'
        'month' in result -> True
        'minutes' in result -> False
        result.dump() -> ['1999', '/', '12', '/', '31']
        - day: 31
        - month: 12
        - year: 1999
    """

    @classmethod
    def new_instance(cls, result_type, toklist, isinstance=isinstance):
        if not toklist:
            return ParseResults(result_type, [], isinstance)
        elif isinstance(toklist, ParseResults):
            if not isinstance(result_type, Forward):
                toklist.type_for_result = result_type
            return toklist
        elif toklist is None:
            Log.error("no longer accepted")
        elif isinstance(toklist, list):
            if len(toklist) != 1:
                Log.error("do not know how to handle")
            return toklist[0]
        elif isinstance(toklist, _generatorType):
            return ParseResults(result_type, list(toklist), isinstance)
        else:
            return ParseResults(result_type, [toklist], isinstance)

    __slots__ = ["tokens_for_result", "type_for_result", "__parent"]

    @property
    def name_for_result(self):
        return get_name(self)

    # Performance tuning: we construct a *lot* of these, so keep this
    # constructor as small and fast as possible
    def __init__(self, result_type, toklist=None, name=None, isinstance=isinstance):
        if not isinstance(result_type, ParserElement):
            Log.error("not expected")
        if isinstance(result_type, Forward):
            Log.error("not expected")
        if isinstance(toklist, ParseResults) or not isinstance(toklist, list):
            Log.error("no longer accepted")

        self.tokens_for_result = toklist
        self.type_for_result = result_type
        self.__parent = None

    def __getitem__(self, i):
        if isinstance(i, (int, slice)):
            for ii, v in enumerate(self):
                if i == ii:
                    return v
        else:
            for tok in get_tokens(self):
                if get_name(tok) == i:
                    return tok

    # def __setitem__(self, k, v, isinstance=isinstance):
    #     if isinstance(k, (int, slice)):
    #         get_tokens(self)[k] = v
    #     else:
    #         for i, v in enumerate(get_tokens(self)):
    #             if get_name(v) == k:
    #                 get_tokens(self)[i] = v
    #                 break
    #         else:
    #             Log.error("not expected")
    #     if isinstance(v, ParseResults):
    #         v.__parent = wkref(self)
    #
    # def __delitem__(self, i):
    #     if isinstance(i, (int, slice)):
    #         del get_tokens(self)[i]
    #     else:
    #         self.tokens_for_result = [r for r in get_tokens(self) if get_name(r) != i]

    def __contains__(self, k):
        return any(get_name(r) == k for r in get_tokens(self))

    def __len__(self):
        return len(get_tokens(self))

    def __bool__(self):
        return (not not get_tokens(self))
    __nonzero__ = __bool__

    def __iter__(self):
        if isinstance(self.type_for_result, Group):
            yield [mm for r in get_tokens(self) for mm in r]
        else:
            for r in get_tokens(self):
                if isinstance(r, ParseResults):
                    for mm in r:
                        yield mm
                else:
                    yield r

    def __reversed__(self):
        return reversed(get_tokens(self))

    def _iterkeys(self):
        return (get_name(r) for r in get_tokens(self) if get_name(r) is not None)

    def _itervalues(self):
        return (r for r in get_tokens(self) if get_name(r) is not None)

    def _iteritems(self):
        return ((n, r) for r in get_tokens(self) for n in [get_name(r)] if n is not None)

    if PY_3:
        keys = _iterkeys
        """Returns an iterator of all named result keys."""

        values = _itervalues
        """Returns an iterator of all named result values."""

        items = _iteritems
        """Returns an iterator of all named result key-value tuples."""

    else:
        iterkeys = _iterkeys
        """Returns an iterator of all named result keys (Python 2.x only)."""

        itervalues = _itervalues
        """Returns an iterator of all named result values (Python 2.x only)."""

        iteritems = _iteritems
        """Returns an iterator of all named result key-value tuples (Python 2.x only)."""

        def keys(self):
            """Returns all named result keys (as a list in Python 2.x, as an iterator in Python 3.x)."""
            return list(self.iterkeys())

        def values(self):
            """Returns all named result values (as a list in Python 2.x, as an iterator in Python 3.x)."""
            return list(self.itervalues())

        def items(self):
            """Returns all named result key-values (as a list of tuples in Python 2.x, as an iterator in Python 3.x)."""
            return list(self.iteritems())

    def haskeys(self):
        """Since keys() returns an iterator, this method is helpful in bypassing
           code that looks for the existence of any defined results names."""
        return any(get_name(r) for r in get_tokens(self))

    def pop(self, *args, **kwargs):
        """
        Removes and returns item at specified index (default= ``last``).
        Supports both ``list`` and ``dict`` semantics for ``pop()``. If
        passed no argument or an integer argument, it will use ``list``
        semantics and pop tokens from the list of parsed tokens. If passed
        a non-integer argument (most likely a string), it will use ``dict``
        semantics and pop the corresponding value from any defined results
        names. A second default return value argument is supported, just as in
        ``dict.pop()``.

        Example::

            def remove_first(tokens):
                tokens.pop(0)
            print(OneOrMore(Word(nums)).parseString("0 123 321")) # -> ['0', '123', '321']
            print(OneOrMore(Word(nums)).addParseAction(remove_first).parseString("0 123 321")) # -> ['123', '321']

            label = Word(alphas)
            patt = label("LABEL") + OneOrMore(Word(nums))
            print(patt.parseString("AAB 123 321").dump())

            # Use pop() in a parse action to remove named result (note that corresponding value is not
            # removed from list form of results)
            def remove_LABEL(tokens):
                tokens.pop("LABEL")
                return tokens
            patt.addParseAction(remove_LABEL)
            print(patt.parseString("AAB 123 321").dump())

        prints::

            ['AAB', '123', '321']
            - LABEL: AAB

            ['AAB', '123', '321']
        """
        if not args:
            args = [-1]
        for k, v in kwargs.items():
            if k == 'default':
                args = (args[0], v)
            else:
                raise TypeError("pop() got an unexpected keyword argument '%s'" % k)
        if (isinstance(args[0], int)
                or len(args) == 1
                or args[0] in self):
            index = args[0]
            ret = self[index]
            del self[index]
            return ret
        else:
            defaultvalue = args[1]
            return defaultvalue

    def get(self, key, defaultValue=None):
        """
        Returns named result matching the given key, or if there is no
        such name, then returns the given ``defaultValue`` or ``None`` if no
        ``defaultValue`` is specified.

        Similar to ``dict.get()``.

        Example::

            integer = Word(nums)
            date_str = integer("year") + '/' + integer("month") + '/' + integer("day")

            result = date_str.parseString("1999/12/31")
            print(result.get("year")) # -> '1999'
            print(result.get("hour", "not specified")) # -> 'not specified'
            print(result.get("hour")) # -> None
        """
        if key in self:
            return self[key]
        else:
            return defaultValue

    def insert(self, index, insStr):
        """
        Inserts new element at location index in the list of parsed tokens.

        Similar to ``list.insert()``.

        Example::

            print(OneOrMore(Word(nums)).parseString("0 123 321")) # -> ['0', '123', '321']

            # use a parse action to insert the parse location in the front of the parsed results
            def insert_locn(locn, tokens):
                tokens.insert(0, locn)
            print(OneOrMore(Word(nums)).addParseAction(insert_locn).parseString("0 123 321")) # -> [0, '0', '123', '321']
        """
        get_tokens(self).insert(index, insStr)

    def append(self, item):
        """
        Add single element to end of ParseResults list of elements.

        Example::

            print(OneOrMore(Word(nums)).parseString("0 123 321")) # -> ['0', '123', '321']

            # use a parse action to compute the sum of the parsed integers, and add it to the end
            def append_sum(tokens):
                tokens.append(sum(map(int, tokens)))
            print(OneOrMore(Word(nums)).addParseAction(append_sum).parseString("0 123 321")) # -> ['0', '123', '321', 444]
        """
        get_tokens(self).append(item)

    def extend(self, itemseq):
        """
        Add sequence of elements to end of ParseResults list of elements.

        Example::

            patt = OneOrMore(Word(alphas))

            # use a parse action to append the reverse of the matched strings, to make a palindrome
            def make_palindrome(tokens):
                tokens.extend(reversed([t[::-1] for t in tokens]))
                return ''.join(tokens)
            print(patt.addParseAction(make_palindrome).parseString("lskdj sdlkjf lksd")) # -> 'lskdjsdlkjflksddsklfjkldsjdksl'
        """
        if isinstance(itemseq, ParseResults):
            self.__iadd__(itemseq)
        else:
            get_tokens(self).extend(itemseq)

    def clear(self):
        """
        Clear all elements and results names.
        """
        del get_tokens(self)[:]

    def __getattr__(self, name):
        try:
            return self[name]
        except KeyError:
            return ""

    def __add__(self, other):
        ret = self.copy()
        ret += other
        return ret

    def __iadd__(self, other):
        # safe to collapse
        if isinstance(other, list):
            Log.error("not expected")

        if isinstance(other, ParseResults):
            name = get_name(other)
            if isinstance(other.type_for_result, Group):
                get_tokens(self).append(other)
            elif name:
                tokens = get_tokens(self)
                for o in get_tokens(other):
                    if isinstance(o, ParseResults):
                        o.name_for_result = name
                    else:
                        o = ParseResults(self.type_for_result, [o])
                    tokens.append(o)
            else:
                get_tokens(self).extend(other)
        else:
            get_tokens(self).append(other)
        return self

    def __radd__(self, other):
        if isinstance(other, int) and other == 0:
            # useful for merging many ParseResults using sum() builtin
            return self.copy()
        else:
            # this may raise a TypeError - so be it
            return other + self

    def __repr__(self):
        return repr(get_tokens(self))

    def __str__(self):
        # if len(get_tokens(self)) == 1:
        #     return str(get_tokens(self)[0])

        return '[' + ', '.join(_ustr(v) if isinstance(v, ParseResults) else repr(v) for v in get_tokens(self)) + ']'

    def _asStringList(self):
        out = []
        for item in get_tokens(self):
            if isinstance(item, ParseResults):
                out.extend(item._asStringList())
            else:
                out.append(_ustr(item))
        return out

    def asString(self, sep=''):
        return sep.join(self._asStringList())

    def asList(self):
        """
        Returns the parse results as a nested list of matching tokens, all converted to strings.

        Example::

            patt = OneOrMore(Word(alphas))
            result = patt.parseString("sldkj lsdkj sldkj")
            # even though the result prints in string-like form, it is actually a pyparsing ParseResults
            print(type(result), result) # -> <class 'pyparsing.ParseResults'> ['sldkj', 'lsdkj', 'sldkj']

            # Use asList() to create an actual list
            result_list = result.asList()
            print(type(result_list), result_list) # -> <class 'list'> ['sldkj', 'lsdkj', 'sldkj']
        """
        output = list(self)
        if isinstance(self.type_for_result, Group):
            return output
        else:
            return output

    def asDict(self):
        """
        Returns the named parse results as a nested dictionary.

        Example::

            integer = Word(nums)
            date_str = integer("year") + '/' + integer("month") + '/' + integer("day")

            result = date_str.parseString('12/31/1999')
            print(type(result), repr(result)) # -> <class 'pyparsing.ParseResults'> (['12', '/', '31', '/', '1999'], {'day': [('1999', 4)], 'year': [('12', 0)], 'month': [('31', 2)]})

            result_dict = result.asDict()
            print(type(result_dict), repr(result_dict)) # -> <class 'dict'> {'day': '1999', 'year': '12', 'month': '31'}

            # even though a ParseResults supports dict-like access, sometime you just need to have a dict
            import json
            print(json.dumps(result)) # -> Exception: TypeError: ... is not JSON serializable
            print(json.dumps(result.asDict())) # -> {"month": "31", "day": "1999", "year": "12"}
        """
        def toItem(obj):
            # return open list of (k,v) pairs
            if isinstance(obj, ParseResults):
                name = get_name(obj)
                if isinstance(obj.type_for_result, Group):
                    yield name, [obj.asList()]
                elif name:
                    yield name, obj.asList()
                else:
                    for tok in obj.tokens_for_result:
                        for p in toItem(tok):
                            yield p
            else:
                yield None, obj

        acc=[]
        d = {}
        for k, v in toItem(self):
            if k is not None:
                d[k] = v
            acc.append(v)
        if d:
            return d
        else:
            return acc

    def copy(self):
        """
        Returns a new copy of a :class:`ParseResults` object.
        """
        ret = ParseResults(self, get_tokens(self))
        ret.__parent = self.__parent
        return ret

    def asXML(self, doctag=None, namedItemsOnly=False, indent="", formatted=True):
        """
        (Deprecated) Returns the parse results as XML. Tags are created for tokens and lists that have defined results names.
        """
        nl = "\n"
        out = []
        namedItems = dict((i, get_name(r)) for i, r in enumerate(get_tokens(self)) if get_name(r))
        nextLevelIndent = indent + "  "

        # collapse out indents if formatting is not desired
        if not formatted:
            indent = ""
            nextLevelIndent = ""
            nl = ""

        selfTag = None
        if doctag is not None:
            selfTag = doctag
        else:
            if get_name(self):
                selfTag = get_name(self)

        if not selfTag:
            if namedItemsOnly:
                return ""
            else:
                selfTag = "ITEM"

        out += [nl, indent, "<", selfTag, ">"]

        for i, (name, res) in enumerate(get_tokens(self)):
            if isinstance(res, ParseResults):
                if i in namedItems:
                    out += [res.asXML(namedItems[i],
                                      namedItemsOnly and doctag is None,
                                      nextLevelIndent,
                                      formatted)]
                else:
                    out += [res.asXML(None,
                                      namedItemsOnly and doctag is None,
                                      nextLevelIndent,
                                      formatted)]
            else:
                # individual token, see if there is a name for it
                resTag = None
                if i in namedItems:
                    resTag = namedItems[i]
                if not resTag:
                    if namedItemsOnly:
                        continue
                    else:
                        resTag = "ITEM"
                xmlBodyText = _xml_escape(_ustr(res))
                out += [nl, nextLevelIndent, "<", resTag, ">",
                        xmlBodyText,
                                                "</", resTag, ">"]

        out += [nl, indent, "</", selfTag, ">"]
        return "".join(out)

    def __lookup(self, sub):
        for name, value in get_tokens(self):
            if sub is value:
                return name
        return None

    def getName(self):
        r"""
        Returns the results name for this token expression. Useful when several
        different expressions might match at a particular location.

        Example::

            integer = Word(nums)
            ssn_expr = Regex(r"\d\d\d-\d\d-\d\d\d\d")
            house_number_expr = Suppress('#') + Word(nums, alphanums)
            user_data = (Group(house_number_expr)("house_number")
                        | Group(ssn_expr)("ssn")
                        | Group(integer)("age"))
            user_info = OneOrMore(user_data)

            result = user_info.parseString("22 111-22-3333 #221B")
            for item in result:
                print(item.getName(), ':', item[0])

        prints::

            age : 22
            ssn : 111-22-3333
            house_number : 221B
        """
        if get_name(self):
            return get_name(self)
        elif self.__parent:
            par = self.__parent()
            if par:
                return par.__lookup(self)
            else:
                return None
        elif len(get_tokens(self)) == 1:
            return get_name(get_tokens(self)[0])
        else:
            return None

    def dump(self, indent='', full=True, include_list=True, _depth=0):
        """
        Diagnostic method for listing out the contents of
        a :class:`ParseResults`. Accepts an optional ``indent`` argument so
        that this string can be embedded in a nested display of other data.

        Example::

            integer = Word(nums)
            date_str = integer("year") + '/' + integer("month") + '/' + integer("day")

            result = date_str.parseString('12/31/1999')
            print(result.dump())

        prints::

            ['12', '/', '31', '/', '1999']
            - day: 1999
            - month: 31
            - year: 12
        """
        out = []
        NL = '\n'
        if include_list:
            out.append(indent + _ustr(self.asList()))
        else:
            out.append('')

        if full:
            if self.haskeys():
                items = sorted((str(k), v) for k, v in self.items())
                for k, v in items:
                    if out:
                        out.append(NL)
                    out.append("%s%s- %s: " % (indent, ('  ' * _depth), k))
                    if isinstance(v, ParseResults):
                        if v:
                            out.append(v.dump(indent=indent, full=full, include_list=include_list, _depth=_depth + 1))
                        else:
                            out.append(_ustr(v))
                    else:
                        out.append(repr(v))
            elif any(isinstance(vv, ParseResults) for vv in self):
                v = self
                for i, vv in enumerate(v):
                    if isinstance(vv, ParseResults):
                        out.append("\n%s%s[%d]:\n%s%s%s" % (indent,
                                                            ('  ' * (_depth)),
                                                            i,
                                                            indent,
                                                            ('  ' * (_depth + 1)),
                                                            vv.dump(indent=indent,
                                                                    full=full,
                                                                    include_list=include_list,
                                                                    _depth=_depth + 1)))
                    else:
                        out.append("\n%s%s[%d]:\n%s%s%s" % (indent,
                                                            ('  ' * (_depth)),
                                                            i,
                                                            indent,
                                                            ('  ' * (_depth + 1)),
                                                            _ustr(vv)))

        return "".join(out)

    def pprint(self, *args, **kwargs):
        """
        Pretty-printer for parsed results as a list, using the
        `pprint <https://docs.python.org/3/library/pprint.html>`_ module.
        Accepts additional positional or keyword args as defined for
        `pprint.pprint <https://docs.python.org/3/library/pprint.html#pprint.pprint>`_ .

        Example::

            ident = Word(alphas, alphanums)
            num = Word(nums)
            func = Forward()
            term = ident | num | Group('(' + func + ')')
            func <<= ident + Group(Optional(delimitedList(term)))
            result = func.parseString("fna a,b,(fnb c,d,200),100")
            result.pprint(width=40)

        prints::

            ['fna',
             ['a',
              'b',
              ['(', 'fnb', ['c', 'd', '200'], ')'],
              '100']]
        """
        pprint(self.asList(), *args, **kwargs)

    # add support for pickle protocol
    def __getstate__(self):
        return (get_tokens(self),
                (
                 self.__parent is not None and self.__parent() or None,
                 self.type_for_result,
                 get_name(self)))

    def __setstate__(self, state):
        self.tokens_for_result, (par, self.type_for_result) = state
        if par is not None:
            self.__parent = wkref(par)
        else:
            self.__parent = None

    def __getnewargs__(self):
        return self.type_for_result, get_tokens(self), get_name(self), self.__asList

    def __dir__(self):
        return dir(type(self)) + list(self.keys())

    @classmethod
    def from_dict(cls, other, name=None):
        """
        Helper classmethod to construct a ParseResults from a dict, preserving the
        name-value relations as results names. If an optional 'name' argument is
        given, a nested ParseResults will be returned
        """
        def is_iterable(obj):
            try:
                iter(obj)
            except Exception:
                return False
            else:
                if PY_3:
                    return not isinstance(obj, (str, bytes))
                else:
                    return not isinstance(obj, basestring)

        ret = cls([])
        for k, v in other.items():
            if isinstance(v, Mapping):
                ret += cls.from_dict(v, name=k)
            else:
                ret += cls([v], name=k)
        if name is not None:
            ret = cls([ret], name=name)
        return ret

MutableMapping.register(ParseResults)
