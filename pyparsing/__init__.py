# -*- coding: utf-8 -*-
# module pyparsing.py
#
# Copyright (c) 2003-2019  Paul T. McGuire
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
import sys

from pyparsing.exceptions import ParseBaseException, ParseException, ParseFatalException, ParseSyntaxException, RecursiveGrammarException, conditionAsParseAction
from pyparsing.expressions import And, Each, MatchFirst, Or, ParseExpression
from pyparsing.parser.base import ParserElement, _PendingSkip, __diag__
from pyparsing.parser.enhancement import CharsNotIn, Combine, Dict, FollowedBy, Forward, Group, Keyword, Literal, NotAny, OneOrMore, Optional, ParseElementEnhance, PrecededBy, SkipTo, StringEnd, Suppress, Token, TokenConverter, ZeroOrMore
from pyparsing.parser.helpers import alphas8bit, anyCloseTag, anyOpenTag, cStyleComment, commaSeparatedList, commonHTMLEntity, countedArray, cppStyleComment, dblQuotedString, dblSlashComment, delimitedList, dictOf, downcaseTokens, downcaseTokens, empty, empty, hexnums, htmlComment, htmlComment, indentedBlock, infixNotation, javaStyleComment, javaStyleComment, lineEnd, lineEnd, lineStart, lineStart, locatedExpr, makeHTMLTags, makeXMLTags, matchOnlyAtCol, matchPreviousExpr, matchPreviousLiteral, nestedExpr, nums, oneOf, opAssoc, opAssoc, operatorPrecedence, operatorPrecedence, originalTextFor, printables, punc8bit, punc8bit, pyparsing_common, pyparsing_common, pythonStyleComment, pythonStyleComment, quotedString, quotedString, removeQuotes, replaceHTMLEntity, replaceWith, replaceWith, restOfLine, restOfLine, sglQuotedString, sglQuotedString, srange, stringEnd, stringEnd, stringStart, stringStart, tokenMap, tokenMap, ungroup, unicodeString, unicodeString, upcaseTokens, upcaseTokens, \
    withAttribute, withAttribute, withClass
from pyparsing.parser.results import ParseResults
from pyparsing.tokens import CaselessKeyword, CaselessLiteral, Char, CloseMatch, Empty, GoToColumn, LineEnd, LineStart, NoMatch, QuotedString, Regex, StringStart, White, Word, WordEnd, WordStart
from pyparsing.utils import PY_3, _MAX_INT, __compat__, _generatorType, _ustr, alphanums, alphas, basestring, col, filterfalse, hexnums, line, lineno, nullDebugAction, nums, printables, singleArgBuiltins, system_version, unichr, unicode, _trim_arity

__doc__ = (
"""
pyparsing module - Classes and methods to define and execute parsing grammars
=============================================================================

The pyparsing module is an alternative approach to creating and
executing simple grammars, vs. the traditional lex/yacc approach, or the
use of regular expressions.  With pyparsing, you don't need to learn
a new syntax for defining grammars or matching expressions - the parsing
module provides a library of classes that you use to construct the
grammar directly in Python.

Here is a program to parse "Hello, World!" (or any greeting of the form
``"<salutation>, <addressee>!"``), built up using :class:`Word`,
:class:`Literal`, and :class:`And` elements
(the :class:`'+'<ParserElement.__add__>` operators create :class:`And` expressions,
and the strings are auto-converted to :class:`Literal` expressions)::

    from pyparsing import Word, alphas

    # define grammar of a greeting
    greet = Word(alphas) + "," + Word(alphas) + "!"

    hello = "Hello, World!"
    print (hello, "->", greet.parseString(hello))

The program outputs the following::

    Hello, World! -> ['Hello', ',', 'World', '!']

The Python representation of the grammar is quite readable, owing to the
self-explanatory class names, and the use of '+', '|' and '^' operators.

The :class:`ParseResults` object returned from
:class:`ParserElement.parseString` can be
accessed as a nested list, a dictionary, or an object with named
attributes.

The pyparsing module handles some of the problems that are typically
vexing when writing text parsers:

  - extra or missing whitespace (the above program will also handle
    "Hello,World!", "Hello  ,  World  !", etc.)
  - quoted strings
  - embedded comments


Getting Started -
-----------------
Visit the classes :class:`ParserElement` and :class:`ParseResults` to
see the base classes that most other pyparsing
classes inherit from. Use the docstrings for examples of how to:

 - construct literal match expressions from :class:`Literal` and
   :class:`CaselessLiteral` classes
 - construct character word-group expressions using the :class:`Word`
   class
 - see how to create repetitive expressions using :class:`ZeroOrMore`
   and :class:`OneOrMore` classes
 - use :class:`'+'<And>`, :class:`'|'<MatchFirst>`, :class:`'^'<Or>`,
   and :class:`'&'<Each>` operators to combine simple expressions into
   more complex ones
 - associate names with your parsed results using
   :class:`ParserElement.setResultsName`
 - access the parsed data, which is returned as a :class:`ParseResults`
   object
 - find some helpful expression short-cuts like :class:`delimitedList`
   and :class:`oneOf`
 - find more useful common expressions in the :class:`pyparsing_common`
   namespace class
"""
)
__version__ = "2.4.2a1"
__versionTime__ = "24 Jul 2019 05:06 UTC"
__author__ = "Paul McGuire <ptmcg@users.sourceforge.net>"


# ~ sys.stderr.write("testing pyparsing module, version %s, %s\n" % (__version__, __versionTime__))

__all__ = ['__version__', '__versionTime__', '__author__', '__compat__', '__diag__',
           'And', 'CaselessKeyword', 'CaselessLiteral', 'CharsNotIn', 'Combine', 'Dict', 'Each', 'Empty',
           'FollowedBy', 'Forward', 'GoToColumn', 'Group', 'Keyword', 'LineEnd', 'LineStart', 'Literal',
           'PrecededBy', 'MatchFirst', 'NoMatch', 'NotAny', 'OneOrMore', 'OnlyOnce', 'Optional', 'Or',
           'ParseBaseException', 'ParseElementEnhance', 'ParseException', 'ParseExpression', 'ParseFatalException',
           'ParseResults', 'ParseSyntaxException', 'ParserElement', 'QuotedString', 'RecursiveGrammarException',
           'Regex', 'SkipTo', 'StringEnd', 'StringStart', 'Suppress', 'Token', 'TokenConverter',
           'White', 'Word', 'WordEnd', 'WordStart', 'ZeroOrMore', 'Char',
           'alphanums', 'alphas', 'alphas8bit', 'anyCloseTag', 'anyOpenTag', 'cStyleComment', 'col',
           'commaSeparatedList', 'commonHTMLEntity', 'countedArray', 'cppStyleComment', 'dblQuotedString',
           'dblSlashComment', 'delimitedList', 'dictOf', 'downcaseTokens', 'empty', 'hexnums',
           'htmlComment', 'javaStyleComment', 'line', 'lineEnd', 'lineStart', 'lineno',
           'makeHTMLTags', 'makeXMLTags', 'matchOnlyAtCol', 'matchPreviousExpr', 'matchPreviousLiteral',
           'nestedExpr', 'nullDebugAction', 'nums', 'oneOf', 'opAssoc', 'operatorPrecedence', 'printables',
           'punc8bit', 'pythonStyleComment', 'quotedString', 'removeQuotes', 'replaceHTMLEntity',
           'replaceWith', 'restOfLine', 'sglQuotedString', 'srange', 'stringEnd',
           'stringStart', 'traceParseAction', 'unicodeString', 'upcaseTokens', 'withAttribute',
           'indentedBlock', 'originalTextFor', 'ungroup', 'infixNotation', 'locatedExpr', 'withClass',
           'CloseMatch', 'tokenMap', 'pyparsing_common', 'pyparsing_unicode', 'unicode_set',
           'conditionAsParseAction',
           ]




# Only works on Python 3.x - nonlocal is toxic to Python 2 installs
#~ 'decorator to trim function calls to match the arity of the target'
#~ def _trim_arity(func, maxargs=3):
    #~ if func in singleArgBuiltins:
        #~ return lambda s,l,t: func(t)
    #~ limit = 0
    #~ foundArity = False
    #~ def wrapper(*args):
        #~ nonlocal limit,foundArity
        #~ while 1:
            #~ try:
                #~ ret = func(*args[limit:])
                #~ foundArity = True
                #~ return ret
            #~ except TypeError:
                #~ if limit == maxargs or foundArity:
                    #~ raise
                #~ limit += 1
                #~ continue
    #~ return wrapper



class OnlyOnce(object):
    """Wrapper for parse actions, to ensure they are only called once.
    """
    def __init__(self, methodCall):
        self.callable = _trim_arity(methodCall)
        self.called = False
    def __call__(self, s, l, t):
        if not self.called:
            results = self.callable(s, l, t)
            self.called = True
            return results
        raise ParseException(s, l, "")
    def reset(self):
        self.called = False

def traceParseAction(f):
    """Decorator for debugging parse actions.

    When the parse action is called, this decorator will print
    ``">> entering method-name(line:<current_source_line>, <parse_location>, <matched_tokens>)"``.
    When the parse action completes, the decorator will print
    ``"<<"`` followed by the returned value, or any exception that the parse action raised.

    Example::

        wd = Word(alphas)

        @traceParseAction
        def remove_duplicate_chars(tokens):
            return ''.join(sorted(set(''.join(tokens))))

        wds = OneOrMore(wd).setParseAction(remove_duplicate_chars)
        print(wds.parseString("slkdjs sld sldd sdlf sdljf"))

    prints::

        >>entering remove_duplicate_chars(line: 'slkdjs sld sldd sdlf sdljf', 0, (['slkdjs', 'sld', 'sldd', 'sdlf', 'sdljf'], {}))
        <<leaving remove_duplicate_chars (ret: 'dfjkls')
        ['dfjkls']
    """
    f = _trim_arity(f)
    def z(*paArgs):
        thisFunc = f.__name__
        s, l, t = paArgs[-3:]
        if len(paArgs) > 3:
            thisFunc = paArgs[0].__class__.__name__ + '.' + thisFunc
        sys.stderr.write(">>entering %s(line: '%s', %d, %r)\n" % (thisFunc, line(l, s), l, t))
        try:
            ret = f(*paArgs)
        except Exception as exc:
            sys.stderr.write("<<leaving %s (exception: %s)\n" % (thisFunc, exc))
            raise
        sys.stderr.write("<<leaving %s (ret: %r)\n" % (thisFunc, ret))
        return ret
    try:
        z.__name__ = f.__name__
    except AttributeError:
        pass
    return z


class _lazyclassproperty(object):
    def __init__(self, fn):
        self.fn = fn
        self.__doc__ = fn.__doc__
        self.__name__ = fn.__name__

    def __get__(self, obj, cls):
        if cls is None:
            cls = type(obj)
        if not hasattr(cls, '_intern') or any(cls._intern is getattr(superclass, '_intern', [])
                                              for superclass in cls.__mro__[1:]):
            cls._intern = {}
        attrname = self.fn.__name__
        if attrname not in cls._intern:
            cls._intern[attrname] = self.fn(cls)
        return cls._intern[attrname]


class unicode_set(object):
    """
    A set of Unicode characters, for language-specific strings for
    ``alphas``, ``nums``, ``alphanums``, and ``printables``.
    A unicode_set is defined by a list of ranges in the Unicode character
    set, in a class attribute ``_ranges``, such as::

        _ranges = [(0x0020, 0x007e), (0x00a0, 0x00ff),]

    A unicode set can also be defined using multiple inheritance of other unicode sets::

        class CJK(Chinese, Japanese, Korean):
            pass
    """
    _ranges = []

    @classmethod
    def _get_chars_for_ranges(cls):
        ret = []
        for cc in cls.__mro__:
            if cc is unicode_set:
                break
            for rr in cc._ranges:
                ret.extend(range(rr[0], rr[-1] + 1))
        return [unichr(c) for c in sorted(set(ret))]

    @_lazyclassproperty
    def printables(cls):
        "all non-whitespace characters in this range"
        return u''.join(filterfalse(unicode.isspace, cls._get_chars_for_ranges()))

    @_lazyclassproperty
    def alphas(cls):
        "all alphabetic characters in this range"
        return u''.join(filter(unicode.isalpha, cls._get_chars_for_ranges()))

    @_lazyclassproperty
    def nums(cls):
        "all numeric digit characters in this range"
        return u''.join(filter(unicode.isdigit, cls._get_chars_for_ranges()))

    @_lazyclassproperty
    def alphanums(cls):
        "all alphanumeric characters in this range"
        return cls.alphas + cls.nums


class pyparsing_unicode(unicode_set):
    """
    A namespace class for defining common language unicode_sets.
    """
    _ranges = [(32, sys.maxunicode)]

    class Latin1(unicode_set):
        "Unicode set for Latin-1 Unicode Character Range"
        _ranges = [(0x0020, 0x007e), (0x00a0, 0x00ff),]

    class LatinA(unicode_set):
        "Unicode set for Latin-A Unicode Character Range"
        _ranges = [(0x0100, 0x017f),]

    class LatinB(unicode_set):
        "Unicode set for Latin-B Unicode Character Range"
        _ranges = [(0x0180, 0x024f),]

    class Greek(unicode_set):
        "Unicode set for Greek Unicode Character Ranges"
        _ranges = [
            (0x0370, 0x03ff), (0x1f00, 0x1f15), (0x1f18, 0x1f1d), (0x1f20, 0x1f45), (0x1f48, 0x1f4d),
            (0x1f50, 0x1f57), (0x1f59,), (0x1f5b,), (0x1f5d,), (0x1f5f, 0x1f7d), (0x1f80, 0x1fb4), (0x1fb6, 0x1fc4),
            (0x1fc6, 0x1fd3), (0x1fd6, 0x1fdb), (0x1fdd, 0x1fef), (0x1ff2, 0x1ff4), (0x1ff6, 0x1ffe),
        ]

    class Cyrillic(unicode_set):
        "Unicode set for Cyrillic Unicode Character Range"
        _ranges = [(0x0400, 0x04ff)]

    class Chinese(unicode_set):
        "Unicode set for Chinese Unicode Character Range"
        _ranges = [(0x4e00, 0x9fff), (0x3000, 0x303f),]

    class Japanese(unicode_set):
        "Unicode set for Japanese Unicode Character Range, combining Kanji, Hiragana, and Katakana ranges"
        _ranges = []

        class Kanji(unicode_set):
            "Unicode set for Kanji Unicode Character Range"
            _ranges = [(0x4E00, 0x9Fbf), (0x3000, 0x303f),]

        class Hiragana(unicode_set):
            "Unicode set for Hiragana Unicode Character Range"
            _ranges = [(0x3040, 0x309f),]

        class Katakana(unicode_set):
            "Unicode set for Katakana  Unicode Character Range"
            _ranges = [(0x30a0, 0x30ff),]

    class Korean(unicode_set):
        "Unicode set for Korean Unicode Character Range"
        _ranges = [(0xac00, 0xd7af), (0x1100, 0x11ff), (0x3130, 0x318f), (0xa960, 0xa97f), (0xd7b0, 0xd7ff), (0x3000, 0x303f),]

    class CJK(Chinese, Japanese, Korean):
        "Unicode set for combined Chinese, Japanese, and Korean (CJK) Unicode Character Range"
        pass

    class Thai(unicode_set):
        "Unicode set for Thai Unicode Character Range"
        _ranges = [(0x0e01, 0x0e3a), (0x0e3f, 0x0e5b),]

    class Arabic(unicode_set):
        "Unicode set for Arabic Unicode Character Range"
        _ranges = [(0x0600, 0x061b), (0x061e, 0x06ff), (0x0700, 0x077f),]

    class Hebrew(unicode_set):
        "Unicode set for Hebrew Unicode Character Range"
        _ranges = [(0x0590, 0x05ff),]

    class Devanagari(unicode_set):
        "Unicode set for Devanagari Unicode Character Range"
        _ranges = [(0x0900, 0x097f), (0xa8e0, 0xa8ff)]

pyparsing_unicode.Japanese._ranges = (pyparsing_unicode.Japanese.Kanji._ranges
                                      + pyparsing_unicode.Japanese.Hiragana._ranges
                                      + pyparsing_unicode.Japanese.Katakana._ranges)

# define ranges in language character sets
if PY_3:
    setattr(pyparsing_unicode, u"العربية", pyparsing_unicode.Arabic)
    setattr(pyparsing_unicode, u"中文", pyparsing_unicode.Chinese)
    setattr(pyparsing_unicode, u"кириллица", pyparsing_unicode.Cyrillic)
    setattr(pyparsing_unicode, u"Ελληνικά", pyparsing_unicode.Greek)
    setattr(pyparsing_unicode, u"עִברִית", pyparsing_unicode.Hebrew)
    setattr(pyparsing_unicode, u"日本語", pyparsing_unicode.Japanese)
    setattr(pyparsing_unicode.Japanese, u"漢字", pyparsing_unicode.Japanese.Kanji)
    setattr(pyparsing_unicode.Japanese, u"カタカナ", pyparsing_unicode.Japanese.Katakana)
    setattr(pyparsing_unicode.Japanese, u"ひらがな", pyparsing_unicode.Japanese.Hiragana)
    setattr(pyparsing_unicode, u"한국어", pyparsing_unicode.Korean)
    setattr(pyparsing_unicode, u"ไทย", pyparsing_unicode.Thai)
    setattr(pyparsing_unicode, u"देवनागरी", pyparsing_unicode.Devanagari)


if __name__ == "__main__":

    selectToken    = CaselessLiteral("select")
    fromToken      = CaselessLiteral("from")

    ident          = Word(alphas, alphanums + "_$")

    columnName     = delimitedList(ident, ".", combine=True).setParseAction(upcaseTokens)
    columnNameList = Group(delimitedList(columnName)).setName("columns")
    columnSpec     = ('*' | columnNameList)

    tableName      = delimitedList(ident, ".", combine=True).setParseAction(upcaseTokens)
    tableNameList  = Group(delimitedList(tableName)).setName("tables")

    simpleSQL      = selectToken("command") + columnSpec("columns") + fromToken + tableNameList("tables")

    # demo runTests method, including embedded comments in test string
    simpleSQL.runTests("""
        # '*' as column list and dotted table name
        select * from SYS.XYZZY

        # caseless match on "SELECT", and casts back to "select"
        SELECT * from XYZZY, ABC

        # list of column names, and mixed case SELECT keyword
        Select AA,BB,CC from Sys.dual

        # multiple tables
        Select A, B, C from Sys.dual, Table2

        # invalid SELECT keyword - should fail
        Xelect A, B, C from Sys.dual

        # incomplete command - should fail
        Select

        # invalid column name - should fail
        Select ^^^ frox Sys.dual

        """)

    pyparsing_common.number.runTests("""
        100
        -100
        +100
        3.14159
        6.02e23
        1e-12
        """)

    # any int or real number, returned as float
    pyparsing_common.fnumber.runTests("""
        100
        -100
        +100
        3.14159
        6.02e23
        1e-12
        """)

    pyparsing_common.hex_integer.runTests("""
        100
        FF
        """)

    import uuid
    pyparsing_common.uuid.setParseAction(tokenMap(uuid.UUID))
    pyparsing_common.uuid.runTests("""
        12345678-1234-5678-1234-567812345678
        """)
