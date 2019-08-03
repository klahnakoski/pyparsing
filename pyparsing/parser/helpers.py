# encoding: utf-8
from datetime import datetime
import re
import warnings

from pyparsing.exceptions import ParseException
from pyparsing.parser.base import ParserElement
from pyparsing.parser.enhancement import Combine, Dict, FollowedBy, Forward, Group, OneOrMore, Optional, SkipTo, Suppress, TokenConverter, ZeroOrMore
from pyparsing.parser.results import ParseResults
from pyparsing.parser.tokens import CaselessKeyword, CaselessLiteral, CharsNotIn, Empty, Keyword, LineEnd, LineStart, Literal, NoMatch, Regex, StringEnd, StringStart, White, Word, _L, _escapeRegexRangeChars
from pyparsing.utils import Iterable, SimpleNamespace, _bslash, _ustr, alphanums, alphas, basestring, col, hexnums, nums, printables, unichr

# import later
And, MatchFirst = [None]*2



#
# global helpers
#
def delimitedList(expr, delim=",", combine=False):
    """Helper to define a delimited list of expressions - the delimiter
    defaults to ','. By default, the list elements and delimiters can
    have intervening whitespace, and comments, but this can be
    overridden by passing ``combine=True`` in the constructor. If
    ``combine`` is set to ``True``, the matching tokens are
    returned as a single token string, with the delimiters included;
    otherwise, the matching tokens are returned as a list of tokens,
    with the delimiters suppressed.

    Example::

        delimitedList(Word(alphas)).parseString("aa,bb,cc") # -> ['aa', 'bb', 'cc']
        delimitedList(Word(hexnums), delim=':', combine=True).parseString("AA:BB:CC:DD:EE") # -> ['AA:BB:CC:DD:EE']
    """
    dlName = _ustr(expr) + " [" + _ustr(delim) + " " + _ustr(expr) + "]..."
    if combine:
        return Combine(expr + ZeroOrMore(delim + expr)).setName(dlName)
    else:
        return (expr + ZeroOrMore(Suppress(delim) + expr)).setName(dlName)

def countedArray(expr, intExpr=None):
    """Helper to define a counted list of expressions.

    This helper defines a pattern of the form::

        integer expr expr expr...

    where the leading integer tells how many expr expressions follow.
    The matched tokens returns the array of expr tokens as a list - the
    leading count token is suppressed.

    If ``intExpr`` is specified, it should be a pyparsing expression
    that produces an integer value.

    Example::

        countedArray(Word(alphas)).parseString('2 ab cd ef')  # -> ['ab', 'cd']

        # in this parser, the leading integer value is given in binary,
        # '10' indicating that 2 values are in the array
        binaryConstant = Word('01').setParseAction(lambda t: int(t[0], 2))
        countedArray(Word(alphas), intExpr=binaryConstant).parseString('10 ab cd ef')  # -> ['ab', 'cd']
    """
    arrayExpr = Forward()
    def countFieldParseAction(s, l, t):
        n = t[0]
        arrayExpr << (n and Group(And([expr] * n)) or Group(empty))
        return []
    if intExpr is None:
        intExpr = Word(nums).setParseAction(lambda t: int(t[0]))
    else:
        intExpr = intExpr.copy()
    intExpr.setName("arrayLen")
    intExpr.addParseAction(countFieldParseAction, callDuringTry=True)
    return (intExpr + arrayExpr).setName('(len) ' + _ustr(expr) + '...')

def _flatten(L):
    ret = []
    for i in L:
        if isinstance(i, list):
            ret.extend(_flatten(i))
        else:
            ret.append(i)
    return ret

def matchPreviousLiteral(expr):
    """Helper to define an expression that is indirectly defined from
    the tokens matched in a previous expression, that is, it looks for
    a 'repeat' of a previous expression.  For example::

        first = Word(nums)
        second = matchPreviousLiteral(first)
        matchExpr = first + ":" + second

    will match ``"1:1"``, but not ``"1:2"``.  Because this
    matches a previous literal, will also match the leading
    ``"1:1"`` in ``"1:10"``. If this is not desired, use
    :class:`matchPreviousExpr`. Do *not* use with packrat parsing
    enabled.
    """
    rep = Forward()
    def copyTokenToRepeater(s, l, t):
        if t:
            if len(t) == 1:
                rep << t[0]
            else:
                # flatten t tokens
                tflat = _flatten(t.asList())
                rep << And(Literal(tt) for tt in tflat)
        else:
            rep << Empty()
    expr.addParseAction(copyTokenToRepeater, callDuringTry=True)
    rep.setName('(prev) ' + _ustr(expr))
    return rep

def matchPreviousExpr(expr):
    """Helper to define an expression that is indirectly defined from
    the tokens matched in a previous expression, that is, it looks for
    a 'repeat' of a previous expression.  For example::

        first = Word(nums)
        second = matchPreviousExpr(first)
        matchExpr = first + ":" + second

    will match ``"1:1"``, but not ``"1:2"``.  Because this
    matches by expressions, will *not* match the leading ``"1:1"``
    in ``"1:10"``; the expressions are evaluated first, and then
    compared, so ``"1"`` is compared with ``"10"``. Do *not* use
    with packrat parsing enabled.
    """
    rep = Forward()
    e2 = expr.copy()
    rep <<= e2
    def copyTokenToRepeater(s, l, t):
        matchTokens = _flatten(t.asList())
        def mustMatchTheseTokens(s, l, t):
            theseTokens = _flatten(t.asList())
            if theseTokens != matchTokens:
                raise ParseException('', 0, '')
        rep.setParseAction(mustMatchTheseTokens, callDuringTry=True)
    expr.addParseAction(copyTokenToRepeater, callDuringTry=True)
    rep.setName('(prev) ' + _ustr(expr))
    return rep

def oneOf(strs, caseless=False, useRegex=True, asKeyword=False):
    """Helper to quickly define a set of alternative Literals, and makes
    sure to do longest-first testing when there is a conflict,
    regardless of the input order, but returns
    a :class:`MatchFirst` for best performance.

    Parameters:

     - strs - a string of space-delimited literals, or a collection of
       string literals
     - caseless - (default= ``False``) - treat all literals as
       caseless
     - useRegex - (default= ``True``) - as an optimization, will
       generate a Regex object; otherwise, will generate
       a :class:`MatchFirst` object (if ``caseless=True`` or ``asKeyword=True``, or if
       creating a :class:`Regex` raises an exception)
     - asKeyword - (default=``False``) - enforce Keyword-style matching on the
       generated expressions

    Example::

        comp_oper = oneOf("< = > <= >= !=")
        var = Word(alphas)
        number = Word(nums)
        term = var | number
        comparison_expr = term + comp_oper + term
        print(comparison_expr.searchString("B = 12  AA=23 B<=AA AA>12"))

    prints::

        [['B', '=', '12'], ['AA', '=', '23'], ['B', '<=', 'AA'], ['AA', '>', '12']]
    """
    if isinstance(caseless, basestring):
        warnings.warn("More than one string argument passed to oneOf, pass "
                      "choices as a list or space-delimited string", stacklevel=2)

    if caseless:
        isequal = (lambda a, b: a.upper() == b.upper())
        masks = (lambda a, b: b.upper().startswith(a.upper()))
        parseElementClass = CaselessKeyword if asKeyword else CaselessLiteral
    else:
        isequal = (lambda a, b: a == b)
        masks = (lambda a, b: b.startswith(a))
        parseElementClass = Keyword if asKeyword else Literal

    symbols = []
    if isinstance(strs, basestring):
        symbols = strs.split()
    elif isinstance(strs, Iterable):
        symbols = list(strs)
    else:
        warnings.warn("Invalid argument to oneOf, expected string or iterable",
                      SyntaxWarning, stacklevel=2)
    if not symbols:
        return NoMatch()

    if not asKeyword:
        # if not producing keywords, need to reorder to take care to avoid masking
        # longer choices with shorter ones
        i = 0
        while i < len(symbols) - 1:
            cur = symbols[i]
            for j, other in enumerate(symbols[i + 1:]):
                if isequal(other, cur):
                    del symbols[i + j + 1]
                    break
                elif masks(cur, other):
                    del symbols[i + j + 1]
                    symbols.insert(i, other)
                    break
            else:
                i += 1

    if not (caseless or asKeyword) and useRegex:
        # ~ print (strs, "->", "|".join([_escapeRegexChars(sym) for sym in symbols]))
        try:
            if len(symbols) == len("".join(symbols)):
                return Regex("[%s]" % "".join(_escapeRegexRangeChars(sym) for sym in symbols)).setName(' | '.join(symbols))
            else:
                return Regex("|".join(re.escape(sym) for sym in symbols)).setName(' | '.join(symbols))
        except Exception:
            warnings.warn("Exception creating Regex for oneOf, building MatchFirst",
                    SyntaxWarning, stacklevel=2)

    # last resort, just use MatchFirst
    return MatchFirst(parseElementClass(sym) for sym in symbols).setName(' | '.join(symbols))

def dictOf(key, value):
    """Helper to easily and clearly define a dictionary by specifying
    the respective patterns for the key and value.  Takes care of
    defining the :class:`Dict`, :class:`ZeroOrMore`, and
    :class:`Group` tokens in the proper order.  The key pattern
    can include delimiting markers or punctuation, as long as they are
    suppressed, thereby leaving the significant key text.  The value
    pattern can include named results, so that the :class:`Dict` results
    can include named token fields.

    Example::

        text = "shape: SQUARE posn: upper left color: light blue texture: burlap"
        attr_expr = (label + Suppress(':') + OneOrMore(data_word, stopOn=label).setParseAction(' '.join))
        print(OneOrMore(attr_expr).parseString(text).dump())

        attr_label = label
        attr_value = Suppress(':') + OneOrMore(data_word, stopOn=label).setParseAction(' '.join)

        # similar to Dict, but simpler call format
        result = dictOf(attr_label, attr_value).parseString(text)
        print(result.dump())
        print(result['shape'])
        print(result.shape)  # object attribute access works too
        print(result.asDict())

    prints::

        [['shape', 'SQUARE'], ['posn', 'upper left'], ['color', 'light blue'], ['texture', 'burlap']]
        - color: light blue
        - posn: upper left
        - shape: SQUARE
        - texture: burlap
        SQUARE
        SQUARE
        {'color': 'light blue', 'shape': 'SQUARE', 'posn': 'upper left', 'texture': 'burlap'}
    """
    return Dict(OneOrMore(Group(key + value)))

def originalTextFor(expr, asString=True):
    """Helper to return the original, untokenized text for a given
    expression.  Useful to restore the parsed fields of an HTML start
    tag into the raw tag text itself, or to revert separate tokens with
    intervening whitespace back to the original matching input text. By
    default, returns astring containing the original parsed text.

    If the optional ``asString`` argument is passed as
    ``False``, then the return value is
    a :class:`ParseResults` containing any results names that
    were originally matched, and a single token containing the original
    matched text from the input string.  So if the expression passed to
    :class:`originalTextFor` contains expressions with defined
    results names, you must set ``asString`` to ``False`` if you
    want to preserve those results name values.

    Example::

        src = "this is test <b> bold <i>text</i> </b> normal text "
        for tag in ("b", "i"):
            opener, closer = makeHTMLTags(tag)
            patt = originalTextFor(opener + SkipTo(closer) + closer)
            print(patt.searchString(src)[0])

    prints::

        ['<b> bold <i>text</i> </b>']
        ['<i>text</i>']
    """
    locMarker = Empty().setParseAction(lambda s, loc, t: loc)
    endlocMarker = locMarker.copy()
    endlocMarker.callPreparse = False
    matchExpr = locMarker("_original_start") + expr + endlocMarker("_original_end")
    if asString:
        extractText = lambda s, l, t: s[t._original_start: t._original_end]
    else:
        def extractText(s, l, t):
            t[:] = [s[t.pop('_original_start'):t.pop('_original_end')]]
    matchExpr.setParseAction(extractText)
    matchExpr.ignoreExprs = expr.ignoreExprs
    return matchExpr

def ungroup(expr):
    """Helper to undo pyparsing's default grouping of And expressions,
    even if all but one are non-empty.
    """
    return TokenConverter(expr).addParseAction(lambda t: t[0])

def locatedExpr(expr):
    """Helper to decorate a returned token with its starting and ending
    locations in the input string.

    This helper adds the following results names:

     - locn_start = location where matched expression begins
     - locn_end = location where matched expression ends
     - value = the actual parsed results

    Be careful if the input text contains ``<TAB>`` characters, you
    may want to call :class:`ParserElement.parseWithTabs`

    Example::

        wd = Word(alphas)
        for match in locatedExpr(wd).searchString("ljsdf123lksdjjf123lkkjj1222"):
            print(match)

    prints::

        [[0, 'ljsdf', 5]]
        [[8, 'lksdjjf', 15]]
        [[18, 'lkkjj', 23]]
    """
    locator = Empty().setParseAction(lambda s, l, t: l)
    return Group(locator("locn_start") + expr("value") + locator.copy().leaveWhitespace()("locn_end"))


# convenience constants for positional expressions
empty       = Empty().setName("empty")
lineStart   = LineStart().setName("lineStart")
lineEnd     = LineEnd().setName("lineEnd")
stringStart = StringStart().setName("stringStart")
stringEnd   = StringEnd().setName("stringEnd")

def srange(s):
    r"""Helper to easily define string ranges for use in Word
    construction. Borrows syntax from regexp '[]' string range
    definitions::

        srange("[0-9]")   -> "0123456789"
        srange("[a-z]")   -> "abcdefghijklmnopqrstuvwxyz"
        srange("[a-z$_]") -> "abcdefghijklmnopqrstuvwxyz$_"

    The input string must be enclosed in []'s, and the returned string
    is the expanded character set joined into a single string. The
    values enclosed in the []'s may be:

     - a single character
     - an escaped character with a leading backslash (such as ``\-``
       or ``\]``)
     - an escaped hex character with a leading ``'\x'``
       (``\x21``, which is a ``'!'`` character) (``\0x##``
       is also supported for backwards compatibility)
     - an escaped octal character with a leading ``'\0'``
       (``\041``, which is a ``'!'`` character)
     - a range of any of the above, separated by a dash (``'a-z'``,
       etc.)
     - any combination of the above (``'aeiouy'``,
       ``'a-zA-Z0-9_$'``, etc.)
    """
    _expanded = lambda p: p if not isinstance(p, ParseResults) else ''.join(unichr(c) for c in range(ord(p[0]), ord(p[1]) + 1))
    try:
        return "".join(_expanded(part) for part in _reBracketExpr.parseString(s).body)
    except Exception:
        return ""

def matchOnlyAtCol(n):
    """Helper method for defining parse actions that require matching at
    a specific column in the input text.
    """
    def verifyCol(strg, locn, toks):
        if col(locn, strg) != n:
            raise ParseException(strg, locn, "matched token not at column %d" % n)
    return verifyCol

def replaceWith(replStr):
    """Helper method for common parse actions that simply return
    a literal value.  Especially useful when used with
    :class:`transformString<ParserElement.transformString>` ().

    Example::

        num = Word(nums).setParseAction(lambda toks: int(toks[0]))
        na = oneOf("N/A NA").setParseAction(replaceWith(math.nan))
        term = na | num

        OneOrMore(term).parseString("324 234 N/A 234") # -> [324, 234, nan, 234]
    """
    return lambda s, l, t: [replStr]

def removeQuotes(s, l, t):
    """Helper parse action for removing quotation marks from parsed
    quoted strings.

    Example::

        # by default, quotation marks are included in parsed results
        quotedString.parseString("'Now is the Winter of our Discontent'") # -> ["'Now is the Winter of our Discontent'"]

        # use removeQuotes to strip quotation marks from parsed results
        quotedString.setParseAction(removeQuotes)
        quotedString.parseString("'Now is the Winter of our Discontent'") # -> ["Now is the Winter of our Discontent"]
    """
    return t[0][1:-1]

def tokenMap(func, *args):
    """Helper to define a parse action by mapping a function to all
    elements of a ParseResults list. If any additional args are passed,
    they are forwarded to the given function as additional arguments
    after the token, as in
    ``hex_integer = Word(hexnums).setParseAction(tokenMap(int, 16))``,
    which will convert the parsed data to an integer using base 16.

    Example (compare the last to example in :class:`ParserElement.transformString`::

        hex_ints = OneOrMore(Word(hexnums)).setParseAction(tokenMap(int, 16))
        test.runTests(hex_ints, '''
            00 11 22 aa FF 0a 0d 1a
            ''')

        upperword = Word(alphas).setParseAction(tokenMap(str.upper))
        OneOrMore(upperword).runTests('''
            my kingdom for a horse
            ''')

        wd = Word(alphas).setParseAction(tokenMap(str.title))
        OneOrMore(wd).setParseAction(' '.join).runTests('''
            now is the winter of our discontent made glorious summer by this sun of york
            ''')

    prints::

        00 11 22 aa FF 0a 0d 1a
        [0, 17, 34, 170, 255, 10, 13, 26]

        my kingdom for a horse
        ['MY', 'KINGDOM', 'FOR', 'A', 'HORSE']

        now is the winter of our discontent made glorious summer by this sun of york
        ['Now Is The Winter Of Our Discontent Made Glorious Summer By This Sun Of York']
    """
    def pa(s, l, t):
        return [func(tokn, *args) for tokn in t]

    try:
        func_name = getattr(func, '__name__',
                            getattr(func, '__class__').__name__)
    except Exception:
        func_name = str(func)
    pa.__name__ = func_name

    return pa

upcaseTokens = tokenMap(lambda t: _ustr(t).upper())
"""(Deprecated) Helper parse action to convert tokens to upper case.
Deprecated in favor of :class:`pyparsing_common.upcaseTokens`"""

downcaseTokens = tokenMap(lambda t: _ustr(t).lower())
"""(Deprecated) Helper parse action to convert tokens to lower case.
Deprecated in favor of :class:`pyparsing_common.downcaseTokens`"""

def _makeTags(tagStr, xml,
              suppress_LT=Suppress("<"),
              suppress_GT=Suppress(">")):
    """Internal helper to construct opening and closing tag expressions, given a tag name"""
    if isinstance(tagStr, basestring):
        resname = tagStr
        tagStr = Keyword(tagStr, caseless=not xml)
    else:
        resname = tagStr.name

    tagAttrName = Word(alphas, alphanums + "_-:")
    if xml:
        tagAttrValue = dblQuotedString.copy().setParseAction(removeQuotes)
        openTag = (suppress_LT
                   + tagStr("tag")
                   + Dict(ZeroOrMore(Group(tagAttrName + Suppress("=") + tagAttrValue)))
                   + Optional("/", default=[False])("empty").setParseAction(lambda s, l, t: t[0] == '/')
                   + suppress_GT)
    else:
        tagAttrValue = quotedString.copy().setParseAction(removeQuotes) | Word(printables, excludeChars=">")
        openTag = (suppress_LT
                   + tagStr("tag")
                   + Dict(ZeroOrMore(Group(tagAttrName.setParseAction(downcaseTokens)
                                           + Optional(Suppress("=") + tagAttrValue))))
                   + Optional("/", default=[False])("empty").setParseAction(lambda s, l, t: t[0] == '/')
                   + suppress_GT)
    closeTag = Combine(_L("</") + tagStr + ">", adjacent=False)

    openTag.setName("<%s>" % resname)
    # add start<tagname> results name in parse action now that ungrouped names are not reported at two levels
    openTag.addParseAction(lambda t: t.__setitem__("start" + "".join(resname.replace(":", " ").title().split()), t.copy()))
    closeTag = closeTag("end" + "".join(resname.replace(":", " ").title().split())).setName("</%s>" % resname)
    openTag.tag = resname
    closeTag.tag = resname
    openTag.tag_body = SkipTo(closeTag())
    return openTag, closeTag

def makeHTMLTags(tagStr):
    """Helper to construct opening and closing tag expressions for HTML,
    given a tag name. Matches tags in either upper or lower case,
    attributes with namespaces and with quoted or unquoted values.

    Example::

        text = '<td>More info at the <a href="https://github.com/pyparsing/pyparsing/wiki">pyparsing</a> wiki page</td>'
        # makeHTMLTags returns pyparsing expressions for the opening and
        # closing tags as a 2-tuple
        a, a_end = makeHTMLTags("A")
        link_expr = a + SkipTo(a_end)("link_text") + a_end

        for link in link_expr.searchString(text):
            # attributes in the <A> tag (like "href" shown here) are
            # also accessible as named results
            print(link.link_text, '->', link.href)

    prints::

        pyparsing -> https://github.com/pyparsing/pyparsing/wiki
    """
    return _makeTags(tagStr, False)

def makeXMLTags(tagStr):
    """Helper to construct opening and closing tag expressions for XML,
    given a tag name. Matches tags only in the given upper/lower case.

    Example: similar to :class:`makeHTMLTags`
    """
    return _makeTags(tagStr, True)

def withAttribute(*args, **attrDict):
    """Helper to create a validating parse action to be used with start
    tags created with :class:`makeXMLTags` or
    :class:`makeHTMLTags`. Use ``withAttribute`` to qualify
    a starting tag with a required attribute value, to avoid false
    matches on common tags such as ``<TD>`` or ``<DIV>``.

    Call ``withAttribute`` with a series of attribute names and
    values. Specify the list of filter attributes names and values as:

     - keyword arguments, as in ``(align="right")``, or
     - as an explicit dict with ``**`` operator, when an attribute
       name is also a Python reserved word, as in ``**{"class":"Customer", "align":"right"}``
     - a list of name-value tuples, as in ``(("ns1:class", "Customer"), ("ns2:align", "right"))``

    For attribute names with a namespace prefix, you must use the second
    form.  Attribute names are matched insensitive to upper/lower case.

    If just testing for ``class`` (with or without a namespace), use
    :class:`withClass`.

    To verify that the attribute exists, but without specifying a value,
    pass ``withAttribute.ANY_VALUE`` as the value.

    Example::

        html = '''
            <div>
            Some text
            <div type="grid">1 4 0 1 0</div>
            <div type="graph">1,3 2,3 1,1</div>
            <div>this has no type</div>
            </div>

        '''
        div,div_end = makeHTMLTags("div")

        # only match div tag having a type attribute with value "grid"
        div_grid = div().setParseAction(withAttribute(type="grid"))
        grid_expr = div_grid + SkipTo(div | div_end)("body")
        for grid_header in grid_expr.searchString(html):
            print(grid_header.body)

        # construct a match with any div tag having a type attribute, regardless of the value
        div_any_type = div().setParseAction(withAttribute(type=withAttribute.ANY_VALUE))
        div_expr = div_any_type + SkipTo(div | div_end)("body")
        for div_header in div_expr.searchString(html):
            print(div_header.body)

    prints::

        1 4 0 1 0

        1 4 0 1 0
        1,3 2,3 1,1
    """
    if args:
        attrs = args[:]
    else:
        attrs = attrDict.items()
    attrs = [(k, v) for k, v in attrs]
    def pa(s, l, tokens):
        for attrName, attrValue in attrs:
            if attrName not in tokens:
                raise ParseException(s, l, "no matching attribute " + attrName)
            if attrValue != withAttribute.ANY_VALUE and tokens[attrName] != attrValue:
                raise ParseException(s, l, "attribute '%s' has value '%s', must be '%s'" %
                                            (attrName, tokens[attrName], attrValue))
    return pa
withAttribute.ANY_VALUE = object()

def withClass(classname, namespace=''):
    """Simplified version of :class:`withAttribute` when
    matching on a div class - made difficult because ``class`` is
    a reserved word in Python.

    Example::

        html = '''
            <div>
            Some text
            <div class="grid">1 4 0 1 0</div>
            <div class="graph">1,3 2,3 1,1</div>
            <div>this &lt;div&gt; has no class</div>
            </div>

        '''
        div,div_end = makeHTMLTags("div")
        div_grid = div().setParseAction(withClass("grid"))

        grid_expr = div_grid + SkipTo(div | div_end)("body")
        for grid_header in grid_expr.searchString(html):
            print(grid_header.body)

        div_any_type = div().setParseAction(withClass(withAttribute.ANY_VALUE))
        div_expr = div_any_type + SkipTo(div | div_end)("body")
        for div_header in div_expr.searchString(html):
            print(div_header.body)

    prints::

        1 4 0 1 0

        1 4 0 1 0
        1,3 2,3 1,1
    """
    classattr = "%s:class" % namespace if namespace else "class"
    return withAttribute(**{classattr: classname})

opAssoc = SimpleNamespace()
opAssoc.LEFT = object()
opAssoc.RIGHT = object()

def infixNotation(baseExpr, opList, lpar=Suppress('('), rpar=Suppress(')')):
    """Helper method for constructing grammars of expressions made up of
    operators working in a precedence hierarchy.  Operators may be unary
    or binary, left- or right-associative.  Parse actions can also be
    attached to operator expressions. The generated parser will also
    recognize the use of parentheses to override operator precedences
    (see example below).

    Note: if you define a deep operator list, you may see performance
    issues when using infixNotation. See
    :class:`ParserElement.enablePackrat` for a mechanism to potentially
    improve your parser performance.

    Parameters:
     - baseExpr - expression representing the most basic element for the
       nested
     - opList - list of tuples, one for each operator precedence level
       in the expression grammar; each tuple is of the form ``(opExpr,
       numTerms, rightLeftAssoc, parseAction)``, where:

       - opExpr is the pyparsing expression for the operator; may also
         be a string, which will be converted to a Literal; if numTerms
         is 3, opExpr is a tuple of two expressions, for the two
         operators separating the 3 terms
       - numTerms is the number of terms for this operator (must be 1,
         2, or 3)
       - rightLeftAssoc is the indicator whether the operator is right
         or left associative, using the pyparsing-defined constants
         ``opAssoc.RIGHT`` and ``opAssoc.LEFT``.
       - parseAction is the parse action to be associated with
         expressions matching this operator expression (the parse action
         tuple member may be omitted); if the parse action is passed
         a tuple or list of functions, this is equivalent to calling
         ``setParseAction(*fn)``
         (:class:`ParserElement.setParseAction`)
     - lpar - expression for matching left-parentheses
       (default= ``Suppress('(')``)
     - rpar - expression for matching right-parentheses
       (default= ``Suppress(')')``)

    Example::

        # simple example of four-function arithmetic with ints and
        # variable names
        integer = pyparsing_common.signed_integer
        varname = pyparsing_common.identifier

        arith_expr = infixNotation(integer | varname,
            [
            ('-', 1, opAssoc.RIGHT),
            (oneOf('* /'), 2, opAssoc.LEFT),
            (oneOf('+ -'), 2, opAssoc.LEFT),
            ])

        test.runTests(arith_expr, '''
            5+3*6
            (5+3)*6
            -2--11
            ''', fullDump=False)

    prints::

        5+3*6
        [[5, '+', [3, '*', 6]]]

        (5+3)*6
        [[[5, '+', 3], '*', 6]]

        -2--11
        [[['-', 2], '-', ['-', 11]]]
    """
    # captive version of FollowedBy that does not do parse actions or capture results names
    class _FB(FollowedBy):
        def parseImpl(self, instring, loc, doActions=True):
            self.expr.tryParse(instring, loc)
            return loc, ParseResults(self, [])

    ret = Forward()
    lastExpr = baseExpr | (lpar + ret + rpar)
    for i, operDef in enumerate(opList):
        opExpr, arity, rightLeftAssoc, pa = (operDef + (None, ))[:4]
        termName = "%s term" % opExpr if arity < 3 else "%s%s term" % opExpr
        if arity == 3:
            if opExpr is None or len(opExpr) != 2:
                raise ValueError(
                    "if numterms=3, opExpr must be a tuple or list of two expressions")
            opExpr1, opExpr2 = opExpr
        thisExpr = Forward().setName(termName)
        if rightLeftAssoc == opAssoc.LEFT:
            if arity == 1:
                matchExpr = _FB(lastExpr + opExpr) + Group(lastExpr + OneOrMore(opExpr))
            elif arity == 2:
                if opExpr is not None:
                    matchExpr = _FB(lastExpr + opExpr + lastExpr) + Group(lastExpr + OneOrMore(opExpr + lastExpr))
                else:
                    matchExpr = _FB(lastExpr + lastExpr) + Group(lastExpr + OneOrMore(lastExpr))
            elif arity == 3:
                matchExpr = (_FB(lastExpr + opExpr1 + lastExpr + opExpr2 + lastExpr)
                             + Group(lastExpr + opExpr1 + lastExpr + opExpr2 + lastExpr))
            else:
                raise ValueError("operator must be unary (1), binary (2), or ternary (3)")
        elif rightLeftAssoc == opAssoc.RIGHT:
            if arity == 1:
                # try to avoid LR with this extra test
                if not isinstance(opExpr, Optional):
                    opExpr = Optional(opExpr)
                matchExpr = _FB(opExpr.expr + thisExpr) + Group(opExpr + thisExpr)
            elif arity == 2:
                if opExpr is not None:
                    matchExpr = _FB(lastExpr + opExpr + thisExpr) + Group(lastExpr + OneOrMore(opExpr + thisExpr))
                else:
                    matchExpr = _FB(lastExpr + thisExpr) + Group(lastExpr + OneOrMore(thisExpr))
            elif arity == 3:
                matchExpr = (_FB(lastExpr + opExpr1 + thisExpr + opExpr2 + thisExpr)
                             + Group(lastExpr + opExpr1 + thisExpr + opExpr2 + thisExpr))
            else:
                raise ValueError("operator must be unary (1), binary (2), or ternary (3)")
        else:
            raise ValueError("operator must indicate right or left associativity")
        if pa:
            if isinstance(pa, (tuple, list)):
                matchExpr.setParseAction(*pa)
            else:
                matchExpr.setParseAction(pa)
        thisExpr <<= (matchExpr.setName(termName) | lastExpr)
        lastExpr = thisExpr
    ret <<= lastExpr
    return ret

operatorPrecedence = infixNotation
"""(Deprecated) Former name of :class:`infixNotation`, will be
dropped in a future release."""

dblQuotedString = Combine(Regex(r'"(?:[^"\n\r\\]|(?:"")|(?:\\(?:[^x]|x[0-9a-fA-F]+)))*') + '"').setName("string enclosed in double quotes")
sglQuotedString = Combine(Regex(r"'(?:[^'\n\r\\]|(?:'')|(?:\\(?:[^x]|x[0-9a-fA-F]+)))*") + "'").setName("string enclosed in single quotes")
quotedString = Combine(Regex(r'"(?:[^"\n\r\\]|(?:"")|(?:\\(?:[^x]|x[0-9a-fA-F]+)))*') + '"'
                       | Regex(r"'(?:[^'\n\r\\]|(?:'')|(?:\\(?:[^x]|x[0-9a-fA-F]+)))*") + "'").setName("quotedString using single or double quotes")
unicodeString = Combine(_L('u') + quotedString.copy()).setName("unicode string literal")

def nestedExpr(opener="(", closer=")", content=None, ignoreExpr=quotedString.copy()):
    """Helper method for defining nested lists enclosed in opening and
    closing delimiters ("(" and ")" are the default).

    Parameters:
     - opener - opening character for a nested list
       (default= ``"("``); can also be a pyparsing expression
     - closer - closing character for a nested list
       (default= ``")"``); can also be a pyparsing expression
     - content - expression for items within the nested lists
       (default= ``None``)
     - ignoreExpr - expression for ignoring opening and closing
       delimiters (default= :class:`quotedString`)

    If an expression is not provided for the content argument, the
    nested expression will capture all whitespace-delimited content
    between delimiters as a list of separate values.

    Use the ``ignoreExpr`` argument to define expressions that may
    contain opening or closing characters that should not be treated as
    opening or closing characters for nesting, such as quotedString or
    a comment expression.  Specify multiple expressions using an
    :class:`Or` or :class:`MatchFirst`. The default is
    :class:`quotedString`, but if no expressions are to be ignored, then
    pass ``None`` for this argument.

    Example::

        data_type = oneOf("void int short long char float double")
        decl_data_type = Combine(data_type + Optional(Word('*')))
        ident = Word(alphas+'_', alphanums+'_')
        number = pyparsing_common.number
        arg = Group(decl_data_type + ident)
        LPAR, RPAR = map(Suppress, "()")

        code_body = nestedExpr('{', '}', ignoreExpr=(quotedString | cStyleComment))

        c_function = (decl_data_type("type")
                      + ident("name")
                      + LPAR + Optional(delimitedList(arg), [])("args") + RPAR
                      + code_body("body"))
        c_function.ignore(cStyleComment)

        source_code = '''
            int is_odd(int x) {
                return (x%2);
            }

            int dec_to_hex(char hchar) {
                if (hchar >= '0' && hchar <= '9') {
                    return (ord(hchar)-ord('0'));
                } else {
                    return (10+ord(hchar)-ord('A'));
                }
            }
        '''
        for func in c_function.searchString(source_code):
            print("%(name)s (%(type)s) args: %(args)s" % func)


    prints::

        is_odd (int) args: [['int', 'x']]
        dec_to_hex (int) args: [['char', 'hchar']]
    """
    if opener == closer:
        raise ValueError("opening and closing strings cannot be the same")
    if content is None:
        if isinstance(opener, basestring) and isinstance(closer, basestring):
            if len(opener) == 1 and len(closer) == 1:
                if ignoreExpr is not None:
                    content = (Combine(OneOrMore(~ignoreExpr
                                                 + CharsNotIn(opener
                                                              + closer
                                                              + ParserElement.DEFAULT_WHITE_CHARS, exact=1)
                                                 )
                                       ).setParseAction(lambda t: t[0].strip()))
                else:
                    content = (empty.copy() + CharsNotIn(opener
                                                         + closer
                                                         + ParserElement.DEFAULT_WHITE_CHARS
                                                         ).setParseAction(lambda t: t[0].strip()))
            else:
                if ignoreExpr is not None:
                    content = (Combine(OneOrMore(~ignoreExpr
                                                 + ~Literal(opener)
                                                 + ~Literal(closer)
                                                 + CharsNotIn(ParserElement.DEFAULT_WHITE_CHARS, exact=1))
                                       ).setParseAction(lambda t: t[0].strip()))
                else:
                    content = (Combine(OneOrMore(~Literal(opener)
                                                 + ~Literal(closer)
                                                 + CharsNotIn(ParserElement.DEFAULT_WHITE_CHARS, exact=1))
                                       ).setParseAction(lambda t: t[0].strip()))
        else:
            raise ValueError("opening and closing arguments must be strings if no content expression is given")
    ret = Forward()
    if ignoreExpr is not None:
        ret <<= Group(Suppress(opener) + ZeroOrMore(ignoreExpr | ret | content) + Suppress(closer))
    else:
        ret <<= Group(Suppress(opener) + ZeroOrMore(ret | content) + Suppress(closer))
    ret.setName('nested %s%s expression' % (opener, closer))
    return ret

def indentedBlock(blockStatementExpr, indentStack, indent=True):
    """Helper method for defining space-delimited indentation blocks,
    such as those used to define block statements in Python source code.

    Parameters:

     - blockStatementExpr - expression defining syntax of statement that
       is repeated within the indented block
     - indentStack - list created by caller to manage indentation stack
       (multiple statementWithIndentedBlock expressions within a single
       grammar should share a common indentStack)
     - indent - boolean indicating whether block must be indented beyond
       the current level; set to False for block of left-most
       statements (default= ``True``)

    A valid block must contain at least one ``blockStatement``.

    Example::

        data = '''
        def A(z):
          A1
          B = 100
          G = A2
          A2
          A3
        B
        def BB(a,b,c):
          BB1
          def BBA():
            bba1
            bba2
            bba3
        C
        D
        def spam(x,y):
             def eggs(z):
                 pass
        '''


        indentStack = [1]
        stmt = Forward()

        identifier = Word(alphas, alphanums)
        funcDecl = ("def" + identifier + Group("(" + Optional(delimitedList(identifier)) + ")") + ":")
        func_body = indentedBlock(stmt, indentStack)
        funcDef = Group(funcDecl + func_body)

        rvalue = Forward()
        funcCall = Group(identifier + "(" + Optional(delimitedList(rvalue)) + ")")
        rvalue << (funcCall | identifier | Word(nums))
        assignment = Group(identifier + "=" + rvalue)
        stmt << (funcDef | assignment | identifier)

        module_body = OneOrMore(stmt)

        parseTree = module_body.parseString(data)
        parseTree.pprint()

    prints::

        [['def',
          'A',
          ['(', 'z', ')'],
          ':',
          [['A1'], [['B', '=', '100']], [['G', '=', 'A2']], ['A2'], ['A3']]],
         'B',
         ['def',
          'BB',
          ['(', 'a', 'b', 'c', ')'],
          ':',
          [['BB1'], [['def', 'BBA', ['(', ')'], ':', [['bba1'], ['bba2'], ['bba3']]]]]],
         'C',
         'D',
         ['def',
          'spam',
          ['(', 'x', 'y', ')'],
          ':',
          [[['def', 'eggs', ['(', 'z', ')'], ':', [['pass']]]]]]]
    """
    backup_stack = indentStack[:]

    def reset_stack():
        indentStack[:] = backup_stack

    def checkPeerIndent(s, l, t):
        if l >= len(s): return
        curCol = col(l, s)
        if curCol != indentStack[-1]:
            if curCol > indentStack[-1]:
                raise ParseException(s, l, "illegal nesting")
            raise ParseException(s, l, "not a peer entry")

    def checkSubIndent(s, l, t):
        curCol = col(l, s)
        if curCol > indentStack[-1]:
            indentStack.append(curCol)
        else:
            raise ParseException(s, l, "not a subentry")

    def checkUnindent(s, l, t):
        if l >= len(s): return
        curCol = col(l, s)
        if not(indentStack and curCol in indentStack):
            raise ParseException(s, l, "not an unindent")
        if curCol < indentStack[-1]:
            indentStack.pop()

    NL = OneOrMore(LineEnd().setWhitespaceChars("\t ").suppress())
    INDENT = (Empty() + Empty().setParseAction(checkSubIndent)).setName('INDENT')
    PEER   = Empty().setParseAction(checkPeerIndent).setName('')
    UNDENT = Empty().setParseAction(checkUnindent).setName('UNINDENT')
    if indent:
        smExpr = Group(Optional(NL)
                       + INDENT
                       + OneOrMore(PEER + Group(blockStatementExpr) + Optional(NL))
                       + UNDENT)
    else:
        smExpr = Group(Optional(NL)
                       + OneOrMore(PEER + Group(blockStatementExpr) + Optional(NL))
                       + UNDENT)
    smExpr.setFailAction(lambda a, b, c, d: reset_stack())
    blockStatementExpr.ignore(_bslash + LineEnd())
    return smExpr.setName('indented block')

alphas8bit = srange(r"[\0xc0-\0xd6\0xd8-\0xf6\0xf8-\0xff]")
punc8bit = srange(r"[\0xa1-\0xbf\0xd7\0xf7]")

anyOpenTag, anyCloseTag = makeHTMLTags(Word(alphas, alphanums + "_:").setName('any tag'))
_htmlEntityMap = dict(zip("gt lt amp nbsp quot apos".split(), '><& "\''))
commonHTMLEntity = Regex('&(?P<entity>' + '|'.join(_htmlEntityMap.keys()) +");").setName("common HTML entity")
def replaceHTMLEntity(t):
    """Helper parser action to replace common HTML entities with their special characters"""
    return _htmlEntityMap.get(t.entity)

# it's easy to get these comment structures wrong - they're very common, so may as well make them available
cStyleComment = Combine(Regex(r"/\*(?:[^*]|\*(?!/))*") + '*/').setName("C style comment")
"Comment of the form ``/* ... */``"

htmlComment = Regex(r"<!--[\s\S]*?-->").setName("HTML comment")
"Comment of the form ``<!-- ... -->``"

restOfLine = Regex(r".*").leaveWhitespace().setName("rest of line")
dblSlashComment = Regex(r"//(?:\\\n|[^\n])*").setName("// comment")
"Comment of the form ``// ... (to end of line)``"

cppStyleComment = Combine(Regex(r"/\*(?:[^*]|\*(?!/))*") + '*/' | dblSlashComment).setName("C++ style comment")
"Comment of either form :class:`cStyleComment` or :class:`dblSlashComment`"

javaStyleComment = cppStyleComment
"Same as :class:`cppStyleComment`"

pythonStyleComment = Regex(r"#.*").setName("Python style comment")
"Comment of the form ``# ... (to end of line)``"

_commasepitem = Combine(OneOrMore(Word(printables, excludeChars=',')
                                  + Optional(Word(" \t")
                                             + ~Literal(",") + ~LineEnd()))).streamline().setName("commaItem")
commaSeparatedList = delimitedList(Optional(quotedString.copy() | _commasepitem, default="")).setName("commaSeparatedList")
"""(Deprecated) Predefined expression of 1 or more printable words or
quoted strings, separated by commas.

This expression is deprecated in favor of :class:`pyparsing_common.comma_separated_list`.
"""

# some other useful expressions - using lower-case class name since we are really using this as a namespace
class pyparsing_common:
    """Here are some common low-level expressions that may be useful in
    jump-starting parser development:

     - numeric forms (:class:`integers<integer>`, :class:`reals<real>`,
       :class:`scientific notation<sci_real>`)
     - common :class:`programming identifiers<identifier>`
     - network addresses (:class:`MAC<mac_address>`,
       :class:`IPv4<ipv4_address>`, :class:`IPv6<ipv6_address>`)
     - ISO8601 :class:`dates<iso8601_date>` and
       :class:`datetime<iso8601_datetime>`
     - :class:`UUID<uuid>`
     - :class:`comma-separated list<comma_separated_list>`

    Parse actions:

     - :class:`convertToInteger`
     - :class:`convertToFloat`
     - :class:`convertToDate`
     - :class:`convertToDatetime`
     - :class:`stripHTMLTags`
     - :class:`upcaseTokens`
     - :class:`downcaseTokens`

    Example::

        test.runTests(pyparsing_common.number, '''
            # any int or real number, returned as the appropriate type
            100
            -100
            +100
            3.14159
            6.02e23
            1e-12
            ''')

        test.runTests(pyparsing_common.fnumber, '''
            # any int or real number, returned as float
            100
            -100
            +100
            3.14159
            6.02e23
            1e-12
            ''')

        test.runTests(pyparsing_common.hex_integer, '''
            # hex numbers
            100
            FF
            ''')

        test.runTests(pyparsing_common.fraction, '''
            # fractions
            1/2
            -3/4
            ''')

        test.runTests(pyparsing_common.mixed_integer, '''
            # mixed fractions
            1
            1/2
            -3/4
            1-3/4
            ''')

        import uuid
        pyparsing_common.uuid.setParseAction(tokenMap(uuid.UUID))
        test.runTests(pyparsing_common.uuid, '''
            # uuid
            12345678-1234-5678-1234-567812345678
            ''')

    prints::

        # any int or real number, returned as the appropriate type
        100
        [100]

        -100
        [-100]

        +100
        [100]

        3.14159
        [3.14159]

        6.02e23
        [6.02e+23]

        1e-12
        [1e-12]

        # any int or real number, returned as float
        100
        [100.0]

        -100
        [-100.0]

        +100
        [100.0]

        3.14159
        [3.14159]

        6.02e23
        [6.02e+23]

        1e-12
        [1e-12]

        # hex numbers
        100
        [256]

        FF
        [255]

        # fractions
        1/2
        [0.5]

        -3/4
        [-0.75]

        # mixed fractions
        1
        [1]

        1/2
        [0.5]

        -3/4
        [-0.75]

        1-3/4
        [1.75]

        # uuid
        12345678-1234-5678-1234-567812345678
        [UUID('12345678-1234-5678-1234-567812345678')]
    """

    convertToInteger = tokenMap(int)
    """
    Parse action for converting parsed integers to Python int
    """

    convertToFloat = tokenMap(float)
    """
    Parse action for converting parsed numbers to Python float
    """

    integer = Word(nums).setName("integer").setParseAction(convertToInteger)
    """expression that parses an unsigned integer, returns an int"""

    hex_integer = Word(hexnums).setName("hex integer").setParseAction(tokenMap(int, 16))
    """expression that parses a hexadecimal integer, returns an int"""

    signed_integer = Regex(r'[+-]?\d+').setName("signed integer").setParseAction(convertToInteger)
    """expression that parses an integer with optional leading sign, returns an int"""

    fraction = (signed_integer().setParseAction(convertToFloat) + '/' + signed_integer().setParseAction(convertToFloat)).setName("fraction")
    """fractional expression of an integer divided by an integer, returns a float"""
    fraction.addParseAction(lambda t: t[0]/t[-1])

    mixed_integer = (fraction | signed_integer + Optional(Optional('-').suppress() + fraction)).setName("fraction or mixed integer-fraction")
    """mixed integer of the form 'integer - fraction', with optional leading integer, returns float"""
    mixed_integer.addParseAction(sum)

    real = Regex(r'[+-]?(:?\d+\.\d*|\.\d+)').setName("real number").setParseAction(convertToFloat)
    """expression that parses a floating point number and returns a float"""

    sci_real = Regex(r'[+-]?(:?\d+(:?[eE][+-]?\d+)|(:?\d+\.\d*|\.\d+)(:?[eE][+-]?\d+)?)').setName("real number with scientific notation").setParseAction(convertToFloat)
    """expression that parses a floating point number with optional
    scientific notation and returns a float"""

    # streamlining this expression makes the docs nicer-looking
    number = (sci_real | real | signed_integer).streamline()
    """any numeric expression, returns the corresponding Python type"""

    fnumber = Regex(r'[+-]?\d+\.?\d*([eE][+-]?\d+)?').setName("fnumber").setParseAction(convertToFloat)
    """any int or real number, returned as float"""

    identifier = Word(alphas + '_', alphanums + '_').setName("identifier")
    """typical code identifier (leading alpha or '_', followed by 0 or more alphas, nums, or '_')"""

    ipv4_address = Regex(r'(25[0-5]|2[0-4][0-9]|1?[0-9]{1,2})(\.(25[0-5]|2[0-4][0-9]|1?[0-9]{1,2})){3}').setName("IPv4 address")
    "IPv4 address (``0.0.0.0 - 255.255.255.255``)"

    _ipv6_part = Regex(r'[0-9a-fA-F]{1,4}').setName("hex_integer")
    _full_ipv6_address = (_ipv6_part + (':' + _ipv6_part) * 7).setName("full IPv6 address")
    _short_ipv6_address = (Optional(_ipv6_part + (':' + _ipv6_part) * (0, 6))
                           + "::"
                           + Optional(_ipv6_part + (':' + _ipv6_part) * (0, 6))
                           ).setName("short IPv6 address")
    _short_ipv6_address.addCondition(lambda t: sum(1 for tt in t if pyparsing_common._ipv6_part.matches(tt)) < 8)
    _mixed_ipv6_address = ("::ffff:" + ipv4_address).setName("mixed IPv6 address")
    ipv6_address = Combine((_full_ipv6_address | _mixed_ipv6_address | _short_ipv6_address).setName("IPv6 address")).setName("IPv6 address")
    "IPv6 address (long, short, or mixed form)"

    mac_address = Regex(r'[0-9a-fA-F]{2}([:.-])[0-9a-fA-F]{2}(?:\1[0-9a-fA-F]{2}){4}').setName("MAC address")
    "MAC address xx:xx:xx:xx:xx (may also have '-' or '.' delimiters)"

    @staticmethod
    def convertToDate(fmt="%Y-%m-%d"):
        """
        Helper to create a parse action for converting parsed date string to Python datetime.date

        Params -
         - fmt - format to be passed to datetime.strptime (default= ``"%Y-%m-%d"``)

        Example::

            date_expr = pyparsing_common.iso8601_date.copy()
            date_expr.setParseAction(pyparsing_common.convertToDate())
            print(date_expr.parseString("1999-12-31"))

        prints::

            [datetime.date(1999, 12, 31)]
        """
        def cvt_fn(s, l, t):
            try:
                return datetime.strptime(t[0], fmt).date()
            except ValueError as ve:
                raise ParseException(s, l, str(ve))
        return cvt_fn

    @staticmethod
    def convertToDatetime(fmt="%Y-%m-%dT%H:%M:%S.%f"):
        """Helper to create a parse action for converting parsed
        datetime string to Python datetime.datetime

        Params -
         - fmt - format to be passed to datetime.strptime (default= ``"%Y-%m-%dT%H:%M:%S.%f"``)

        Example::

            dt_expr = pyparsing_common.iso8601_datetime.copy()
            dt_expr.setParseAction(pyparsing_common.convertToDatetime())
            print(dt_expr.parseString("1999-12-31T23:59:59.999"))

        prints::

            [datetime.datetime(1999, 12, 31, 23, 59, 59, 999000)]
        """
        def cvt_fn(s, l, t):
            try:
                return datetime.strptime(t[0], fmt)
            except ValueError as ve:
                raise ParseException(s, l, str(ve))
        return cvt_fn

    iso8601_date = Regex(r'(?P<year>\d{4})(?:-(?P<month>\d\d)(?:-(?P<day>\d\d))?)?').setName("ISO8601 date")
    "ISO8601 date (``yyyy-mm-dd``)"

    iso8601_datetime = Regex(r'(?P<year>\d{4})-(?P<month>\d\d)-(?P<day>\d\d)[T ](?P<hour>\d\d):(?P<minute>\d\d)(:(?P<second>\d\d(\.\d*)?)?)?(?P<tz>Z|[+-]\d\d:?\d\d)?').setName("ISO8601 datetime")
    "ISO8601 datetime (``yyyy-mm-ddThh:mm:ss.s(Z|+-00:00)``) - trailing seconds, milliseconds, and timezone optional; accepts separating ``'T'`` or ``' '``"

    uuid = Regex(r'[0-9a-fA-F]{8}(-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}').setName("UUID")
    "UUID (``xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx``)"

    _html_stripper = anyOpenTag.suppress() | anyCloseTag.suppress()
    @staticmethod
    def stripHTMLTags(s, l, tokens):
        """Parse action to remove HTML tags from web page HTML source

        Example::

            # strip HTML links from normal text
            text = '<td>More info at the <a href="https://github.com/pyparsing/pyparsing/wiki">pyparsing</a> wiki page</td>'
            td, td_end = makeHTMLTags("TD")
            table_text = td + SkipTo(td_end).setParseAction(pyparsing_common.stripHTMLTags)("body") + td_end
            print(table_text.parseString(text).body)

        Prints::

            More info at the pyparsing wiki page
        """
        return pyparsing_common._html_stripper.transformString(tokens[0])

    _commasepitem = Combine(OneOrMore(~Literal(",")
                                      + ~LineEnd()
                                      + Word(printables, excludeChars=',')
                                      + Optional(White(" \t")))).streamline().setName("commaItem")
    comma_separated_list = delimitedList(Optional(quotedString.copy()
                                                  | _commasepitem, default='')
                                         ).setName("comma separated list")
    """Predefined expression of 1 or more printable words or quoted strings, separated by commas."""

    upcaseTokens = staticmethod(tokenMap(lambda t: _ustr(t).upper()))
    """Parse action to convert tokens to upper case."""

    downcaseTokens = staticmethod(tokenMap(lambda t: _ustr(t).lower()))
    """Parse action to convert tokens to lower case."""


# export
from pyparsing.parser import base

base._flatten = _flatten
base.replaceWith = replaceWith
base.quotedString = quotedString

_escapedPunc = Word(_bslash, r"\[]-*.$+^?()~ ", exact=2).setParseAction(lambda s, l, t: t[0][1])
_escapedHexChar = Regex(r"\\0?[xX][0-9a-fA-F]+").setParseAction(lambda s, l, t: unichr(int(t[0].lstrip(r'\0x'), 16)))
_escapedOctChar = Regex(r"\\0[0-7]+").setParseAction(lambda s, l, t: unichr(int(t[0][1:], 8)))
_singleChar = _escapedPunc | _escapedHexChar | _escapedOctChar | CharsNotIn(r'\]', exact=1)
_charRange = Group(_singleChar + Suppress("-") + _singleChar)
_reBracketExpr = Literal("[") + Optional("^").setResultsName("negate") + Group(OneOrMore(_charRange | _singleChar)).setResultsName("body") + "]"

