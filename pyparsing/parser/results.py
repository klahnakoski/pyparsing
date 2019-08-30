# encoding: utf-8
from collections import Mapping, MutableMapping
from copy import copy
from itertools import count
from pprint import pprint
from weakref import ref as wkref

from mo_logs import Log

from pyparsing.utils import PY_3, _generatorType, _ustr, _xml_escape, basestring, __compat__

Suppress, ParserElement, Forward, Group = [None]*4

_get = object.__getattribute__;

def get_name(tok):
    try:
        if isinstance(tok, Forward):
            return tok.type_for_result.expr.resultsName
        if isinstance(tok, ParseResults):
            return _get(tok, "type_for_result").resultsName
        return None
    except Exception as e:
        raise e


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

    __slots__ = ["tokens_for_result", "type_for_result"]

    @property
    def name_for_result(self):
        return get_name(self)

    # Performance tuning: we construct a *lot* of these, so keep this
    # constructor as small and fast as possible
    def __init__(self, result_type, toklist=None):
        if not isinstance(result_type, ParserElement):
            Log.error("not expected")
        if isinstance(result_type, Forward):
            Log.error("not expected")
        if isinstance(toklist, ParseResults) or not isinstance(toklist, list):
            Log.error("no longer accepted")

        self.tokens_for_result = toklist
        self.type_for_result = result_type

    def _get_item_by_name(self, i):
        # return open list of values
        if not __compat__.collect_all_And_tokens:
            # pre 2.3
            if self.name_for_result == i:
                yield self[0]
            else:
                for tok in self.tokens_for_result:
                    if get_name(tok) == i:
                        yield tok[0]
        else:
            if get_name(self) == i:
                if len(self.tokens_for_result) == 1:
                    yield self.tokens_for_result[0]
                    return
                yield self
            else:
                for tok in self.tokens_for_result:
                    if isinstance(tok, ParseResults):
                        for f in tok._get_item_by_name(i):
                            yield f

    def __getitem__(self, i):
        if not __compat__.collect_all_And_tokens:
            # pre 2.3
            if isinstance(i, int):
                if isinstance(self.type_for_result, Group):
                    return self.tokens_for_result[0][i]
                else:
                    for ii, v in enumerate(self):
                        if i == ii:
                            return v
            elif self.name_for_result == i:
                return self[0]
            else:
                for tok in self.tokens_for_result:
                    if get_name(tok) == i:
                        return tok[0]
        else:
            if isinstance(i, int):
                for ii, v in enumerate(self):
                    if i == ii:
                        return v
            else:
                output = list(self._get_item_by_name(i))
                if len(output) == 0:
                    return None
                elif len(output) == 1:
                    return output[0]
                else:
                    return ParseResults(self.type_for_result, output)
        # Log.error("No name by {{name|quote}}", name=i)

    def __setitem__(self, k, v):
        if isinstance(k, (int, slice)):
            for i, t in enumerate(self.tokens_for_result):
                if isinstance(t, ParseResults):
                    ii = len(t)
                    if k < ii:
                        t[k] = v
                        return
                    else:
                        k -= ii
                else:
                    if k==0:
                        self.tokens_for_result[i] = v
                        return
                    else:
                        k-=1

            Log.error("index {{index}} beyond existing tokens", index=k)
        else:
            for i, vv in enumerate(self.tokens_for_result):
                if get_name(vv) == k:
                    self.tokens_for_result[i] = v
                    break
            else:
                self.tokens_for_result.append(Annotation(k, v))

    # def __delitem__(self, i):
    #     if isinstance(i, (int, slice)):
    #         del self.tokens_for_result[i]
    #     else:
    #         self.tokens_for_result = [r for r in self.tokens_for_result if get_name(r) != i]

    def __contains__(self, k):
        return any(get_name(r) == k for r in self.tokens_for_result)

    def __len__(self):
        if isinstance(self.type_for_result, Group):
            return len(self.tokens_for_result[0])
        else:
            return sum(1 for t in self)

    def __bool__(self):
        return (not not self.tokens_for_result)
    __nonzero__ = __bool__

    def __iter__(self):
        if isinstance(self, Annotation):
            return
            # yield self
        elif isinstance(self.type_for_result, Suppress):
            return
        else:
            for r in self.tokens_for_result:
                if isinstance(r, ParseResults):
                    if isinstance(r, Annotation):
                        return
                    elif isinstance(r.type_for_result, Group):
                        yield r
                    # elif get_name(r):
                    #     yield r
                    elif not isinstance(r.type_for_result, Group):
                        for mm in r:
                            yield mm
                else:
                    yield r

    def __delitem__(self, key):
        if isinstance(key, int):
            if key==-1:
                # DELETE LEFT OR RIGHT
                if not self.tokens_for_result:
                    Log.error("not expected")
                first = self.tokens_for_result[0]
                if isinstance(first, ParseResults):
                    if isinstance(first.type_for_result, Group):
                        self.tokens_for_result = self.tokens_for_result[1:]
                        return
                    else:
                        del first[-1]
                        return
                else:
                    self.tokens_for_result = self.tokens_for_result[1:]
                    return

            elif key == 0:
                if isinstance(self.type_for_result, Group):
                    del self.tokens_for_result[0][0]
                    return
                del self[-1]
                return
            else:
                for t in self.tokens_for_result:
                    if key < len(t):
                        del t[key]
                        return
                    key -= len(t)
        else:
            for i, t in enumerate(self.tokens_for_result):
                if get_name(t)==key:
                    del self.tokens_for_result[i]
                    return
            for t in self:
                del t[key]


    def __reversed__(self):
        return reversed(self.tokens_for_result)

    def _iterkeys(self):
        return (get_name(r) for r in self.tokens_for_result if get_name(r) is not None)

    def _itervalues(self):
        return (r for r in self.tokens_for_result if get_name(r) is not None)

    def _iteritems(self):
        return ((n, r) for r in self.tokens_for_result for n in [get_name(r)] if n is not None)

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
        return any(get_name(r) for r in self.tokens_for_result)

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
        self.tokens_for_result.insert(index, insStr)

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
        self.tokens_for_result.append(item)

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
            self.tokens_for_result.extend(itemseq)

    def clear(self):
        """
        Clear all elements and results names.
        """
        Log.error("not expected")
        del self.tokens_for_result[:]

    def __contains__(self, item):
        return self[item] is not None

    def __getattr__(self, name):
        try:
            return self[name]
        except KeyError:
            return ""

    def __add__(self, other):
        Log.error("not expected")
        ret = copy(self)
        ret += other
        return ret

    def __iadd__(self, other):
        Log.error("not expected")
        # safe to collapse
        if isinstance(other, list):
            Log.error("not expected")

        if isinstance(other, ParseResults):
            name = get_name(other)
            if isinstance(other.type_for_result, Group):
                self.tokens_for_result.append(other)
            elif name:
                tokens = self.tokens_for_result
                for o in other.tokens_for_result:
                    if isinstance(o, ParseResults):
                        o.name_for_result = name
                    else:
                        o = ParseResults(self.type_for_result, [o])
                    tokens.append(o)
            else:
                self.tokens_for_result.extend(other)
        else:
            self.tokens_for_result.append(other)
        return self

    def __repr__(self):
        try:
            return repr(self.tokens_for_result)
        except Exception as e:
            Log.warning("problem", cause=e)
            return "[]"

    def __str__(self):
        # if len(self.tokens_for_result) == 1:
        #     return str(self.tokens_for_result[0])

        return '[' + ', '.join(_ustr(v) if isinstance(v, ParseResults) else repr(v) for v in self.tokens_for_result) + ']'

    def _asStringList(self):
        out = []
        for item in self.tokens_for_result:
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

        def internal(obj, depth):
            if depth > 20:
                Log.warning("deep!")

            if isinstance(obj, Annotation):
                return []
            elif isinstance(obj, ParseResults):
                output = []
                for t in obj:
                    inner = internal(t, depth+1)
                    if len(inner) == 0:
                        pass
                    elif len(inner) == 1:
                        output.append(inner[0])
                    else:
                        output.append(inner)
                if isinstance(obj.type_for_result, Group):
                    return [output]
                else:
                    return output
            else:
                return [obj]

        return internal(self, 0)

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
        def items(obj):
            # return open list of (k, list(v)) pairs
            if isinstance(obj, ParseResults):
                name = get_name(obj)

                if isinstance(obj.type_for_result, Group):
                    yield name, [pack(obj.tokens_for_result)]
                elif name:
                    yield name, pack(obj.tokens_for_result)
                elif isinstance(obj.type_for_result, Suppress):
                    # results not found in the original data
                    return
                else:
                    for tok in obj.tokens_for_result:
                        for p in items(tok):
                            yield p
            else:
                yield None, [obj]

        def pack(objs):
            # return a dict, if possible
            # otherwise return an open list
            output_list = []
            output_dict = {}
            for t in objs:
                for k, v in items(t):
                    if k is not None:
                        old_v = output_dict.get(k)
                        if old_v is None:
                            output_dict[k] = v
                        elif isinstance(old_v, list):
                            old_v.extend(v)
                    output_list.extend(v)

            if output_dict:
                # return output_dict
                return [{k: simpler(v) for k, v in output_dict.items()}]
            else:
                return output_list

        return simpler(pack([self]))

    def __copy__(self):
        """
        Returns a new copy of a :class:`ParseResults` object.
        """
        ret = ParseResults(self.type_for_result, list(self.tokens_for_result))
        return ret

    def asXML(self, doctag=None, namedItemsOnly=False, indent="", formatted=True):
        """
        (Deprecated) Returns the parse results as XML. Tags are created for tokens and lists that have defined results names.
        """
        if formatted:
            more_indent = "  "
            nl = "\n"
        else:
            indent = ""
            more_indent = ""
            nl = ""

        no_name = "ITEM"

        def toXML(tag, token, namedItemsOnly, indent):
            """ return open iterator of not-indented tags """
            if isinstance(token, ParseResults):
                name = get_name(token)
                if isinstance(token.type_for_result, Group):
                    yield token.asXML(name or tag, not name, indent, formatted)
                elif name:
                    yield nl + indent + "<" + name + ">"
                    for sub_tok in token.tokens_for_result:
                        for x in toXML(None, sub_tok, False, indent+more_indent):
                            yield x
                    yield "</" + name + ">"
                else:
                    for sub_tok in token.tokens_for_result:
                        for x in toXML(tag, sub_tok, namedItemsOnly, indent):
                            yield x
            elif tag is no_name and namedItemsOnly:
                return
            elif tag:
                yield nl + indent + "<" + tag + ">"
                yield _xml_escape(_ustr(token))
                yield "</" + tag + ">"
            else:
                yield _xml_escape(_ustr(token))

        name = doctag or get_name(self) or no_name

        out = []
        out.append(nl + indent + "<" + name + ">")
        for sub_tok in self.tokens_for_result:
            out.extend(toXML(no_name, sub_tok, namedItemsOnly, indent+more_indent))
        out.append( nl + indent + "</" + name + ">")

        return "".join(out)

    def __lookup(self, sub):
        for name, value in self.tokens_for_result:
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
        elif len(self.tokens_for_result) == 1:
            return get_name(self.tokens_for_result[0])
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
        if _depth > 20:
            Log.warning("not expected")
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
                    if isinstance(vv, Annotation):
                        pass
                    elif isinstance(vv, ParseResults):
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
        parser_type = self.type_for_result
        name = parser_type.resultsName
        type_name = parser_type.__class__.__name__
        return self.tokens_for_result, type_name, name

    def __setstate__(self, state):
        self.tokens_for_result, type_name, name = state
        parser_type = globals().get(type_name, ParserElement)
        parser_element = parser_type(None)
        parser_element.resultsName = name

        self.type_for_result = parser_element

    def __getnewargs__(self):
        old_parser = self.type_for_result
        parser_type = globals().get(old_parser.__class__.__name__, ParserElement)
        new_parser = parser_type(None)
        new_parser.resultsName = old_parser.resultsName
        return new_parser, self.tokens_for_result

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


def simpler(v):
    # convert an open list to object it represents
    if isinstance(v, list):
        if len(v) == 0:
            return None
        elif len(v) == 1:
            return v[0]
    return v


class Annotation(ParseResults):
    # Append one of these to the parse results to
    # add key: value pair not found in the original text

    __slots__=[]

    def __init__(self, name, value):
        ParseResults.__init__(self, Suppress(None)(name), [value])

    def __repr__(self):
        return "{" + get_name(self) + ": ...}"


MutableMapping.register(ParseResults)

