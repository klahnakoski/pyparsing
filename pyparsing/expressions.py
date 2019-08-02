# encoding: utf-8
from operator import itemgetter
import warnings

from mo_logs import Log

from pyparsing.exceptions import ParseBaseException, ParseException, ParseSyntaxException
from pyparsing.parser.base import ParserElement, _PendingSkip, __diag__
from pyparsing.parser.enhancement import OneOrMore, Optional, SkipTo, Suppress, ZeroOrMore, Group
from pyparsing.parser.results import ParseResults
from pyparsing.parser.tokens import Empty
from pyparsing.utils import Iterable, __compat__, _generatorType, _ustr


class ParseExpression(ParserElement):
    """Abstract subclass of ParserElement, for combining and
    post-processing parsed tokens.
    """
    def __init__(self, exprs, savelist=False):
        super(ParseExpression, self).__init__(savelist)

        if isinstance(exprs, _generatorType):
            exprs = list(exprs)
        elif not isinstance(exprs, ParserElement) and isinstance(exprs, Iterable):
            exprs = list(exprs)
        else:
            exprs = [exprs]

        self.exprs = [self.normalize(e) for e in exprs]
        self.callPreparse = False

    def append(self, other):
        self.exprs.append(other)
        self.strRepr = None
        return self

    def leaveWhitespace(self):
        """Extends ``leaveWhitespace`` defined in base class, and also invokes ``leaveWhitespace`` on
           all contained expressions."""
        self.skipWhitespace = False
        self.exprs = [e.copy() for e in self.exprs]
        for e in self.exprs:
            e.leaveWhitespace()
        return self

    def ignore(self, other):
        if isinstance(other, Suppress):
            if other not in self.ignoreExprs:
                super(ParseExpression, self).ignore(other)
                for e in self.exprs:
                    e.ignore(self.ignoreExprs[-1])
        else:
            super(ParseExpression, self).ignore(other)
            for e in self.exprs:
                e.ignore(self.ignoreExprs[-1])
        return self

    def __str__(self):
        try:
            return super(ParseExpression, self).__str__()
        except Exception:
            pass

        if self.strRepr is None:
            self.strRepr = "%s:(%s)" % (self.__class__.__name__, _ustr(self.exprs))
        return self.strRepr

    def streamline(self):
        super(ParseExpression, self).streamline()

        for e in self.exprs:
            e.streamline()

        # collapse nested And's of the form And(And(And(a, b), c), d) to And(a, b, c, d)
        # but only if there are no parse actions or resultsNames on the nested And's
        # (likewise for Or's and MatchFirst's)
        if len(self.exprs) == 2:
            other = self.exprs[0]
            if (isinstance(other, self.__class__)
                    and not other.parseAction
                    and other.resultsName is None
                    and not other.debug):
                self.exprs = other.exprs[:] + [self.exprs[1]]
                self.strRepr = None
                self.mayReturnEmpty |= other.mayReturnEmpty
                self.mayIndexError  |= other.mayIndexError

            other = self.exprs[-1]
            if (isinstance(other, self.__class__)
                    and not other.parseAction
                    and other.resultsName is None
                    and not other.debug):
                self.exprs = self.exprs[:-1] + other.exprs[:]
                self.strRepr = None
                self.mayReturnEmpty |= other.mayReturnEmpty
                self.mayIndexError  |= other.mayIndexError

        self.errmsg = "Expected " + _ustr(self)

        return self

    def validate(self, validateTrace=None):
        tmp = (validateTrace if validateTrace is not None else [])[:] + [self]
        for e in self.exprs:
            e.validate(tmp)
        self.checkRecursion([])

    def copy(self):
        ret = super(ParseExpression, self).copy()
        ret.exprs = [e.copy() for e in self.exprs]
        return ret

    def _setResultsName(self, name, listAllMatches=False):
        if __diag__.warn_ungrouped_named_tokens_in_collection:
            for e in self.exprs:
                if isinstance(e, ParserElement) and e.resultsName:
                    warnings.warn("{0}: setting results name {1!r} on {2} expression "
                                  "collides with {3!r} on contained expression".format("warn_ungrouped_named_tokens_in_collection",
                                                                                       name,
                                                                                       type(self).__name__,
                                                                                       e.resultsName),
                                  stacklevel=3)

        return super(ParseExpression, self)._setResultsName(name, listAllMatches)


class And(ParseExpression):
    """
    Requires all given :class:`ParseExpression` s to be found in the given order.
    Expressions may be separated by whitespace.
    May be constructed using the ``'+'`` operator.
    May also be constructed using the ``'-'`` operator, which will
    suppress backtracking.

    Example::

        integer = Word(nums)
        name_expr = OneOrMore(Word(alphas))

        expr = And([integer("id"), name_expr("name"), integer("age")])
        # more easily written as:
        expr = integer("id") + name_expr("name") + integer("age")
    """

    class _ErrorStop(Empty):
        def __init__(self, *args, **kwargs):
            super(And._ErrorStop, self).__init__(*args, **kwargs)
            self.name = '-'
            self.leaveWhitespace()

    def __init__(self, exprs, savelist=True):
        if exprs and Ellipsis in exprs:
            tmp = []
            for i, expr in enumerate(exprs):
                if expr is Ellipsis:
                    if i < len(exprs) - 1:
                        skipto_arg = (Empty() + exprs[i + 1]).exprs[-1]
                        tmp.append(SkipTo(skipto_arg)("_skipped*"))
                    else:
                        raise Exception("cannot construct And with sequence ending in ...")
                else:
                    tmp.append(expr)
            exprs[:] = tmp
        super(And, self).__init__(exprs, savelist)
        self.mayReturnEmpty = all(e.mayReturnEmpty for e in self.exprs)
        self.setWhitespaceChars(self.exprs[0].whiteChars)
        self.skipWhitespace = self.exprs[0].skipWhitespace
        self.callPreparse = True

    def streamline(self):
        # collapse any _PendingSkip's
        if self.exprs:
            if any(isinstance(e, ParseExpression) and e.exprs and isinstance(e.exprs[-1], _PendingSkip)
                   for e in self.exprs[:-1]):
                for i, e in enumerate(self.exprs[:-1]):
                    if e is None:
                        continue
                    if (isinstance(e, ParseExpression)
                            and e.exprs and isinstance(e.exprs[-1], _PendingSkip)):
                        e.exprs[-1] = e.exprs[-1] + self.exprs[i + 1]
                        self.exprs[i + 1] = None
                self.exprs = [e for e in self.exprs if e is not None]

        super(And, self).streamline()
        self.mayReturnEmpty = all(e.mayReturnEmpty for e in self.exprs)
        return self

    def parseImpl(self, instring, loc, doActions=True):
        # pass False as last arg to _parse for first element, since we already
        # pre-parsed the string as part of our And pre-parsing
        errorStop = False
        acc = []
        for e in self.exprs:
            if isinstance(e, And._ErrorStop):
                errorStop = True
                continue
            if errorStop:
                try:
                    loc, exprtokens = e._parse(instring, loc, doActions)
                except ParseSyntaxException:
                    raise
                except ParseBaseException as pe:
                    raise ParseSyntaxException(pe.pstr, pe.loc, pe.msg, pe.parserElement)
                except IndexError:
                    raise ParseSyntaxException(instring, len(instring), self.errmsg, self)
            else:
                loc, exprtokens = e._parse(instring, loc, doActions)

            if exprtokens.name_for_result:
                Log.error("do not know how to handle")
            elif isinstance(exprtokens.type_for_result, Group):
                acc.append(exprtokens)
            else:
                acc.extend(iter(exprtokens))

        return loc, ParseResults(self, acc)

    def __iadd__(self, other):
        return self.append(self.normalize(other))  # And([self, other])

    def checkRecursion(self, parseElementList):
        subRecCheckList = parseElementList[:] + [self]
        for e in self.exprs:
            e.checkRecursion(subRecCheckList)
            if not e.mayReturnEmpty:
                break

    def __str__(self):
        if hasattr(self, "name"):
            return self.name

        if self.strRepr is None:
            self.strRepr = "{" + " ".join(_ustr(e) for e in self.exprs) + "}"

        return self.strRepr


class Or(ParseExpression):
    """Requires that at least one :class:`ParseExpression` is found. If
    two expressions match, the expression that matches the longest
    string will be used. May be constructed using the ``'^'``
    operator.

    Example::

        # construct Or using '^' operator

        number = Word(nums) ^ Combine(Word(nums) + '.' + Word(nums))
        print(number.searchString("123 3.1416 789"))

    prints::

        [['123'], ['3.1416'], ['789']]
    """
    def __init__(self, exprs, savelist=False):
        super(Or, self).__init__(exprs, savelist)
        if self.exprs:
            self.mayReturnEmpty = any(e.mayReturnEmpty for e in self.exprs)
        else:
            self.mayReturnEmpty = True

    def streamline(self):
        super(Or, self).streamline()
        if __compat__.collect_all_And_tokens:
            self.saveAsList = any(e.saveAsList for e in self.exprs)
        return self

    def parseImpl(self, instring, loc, doActions=True):
        maxExcLoc = -1
        maxException = None
        matches = []
        for e in self.exprs:
            try:
                loc2 = e.tryParse(instring, loc)
            except ParseException as err:
                err.__traceback__ = None
                if err.loc > maxExcLoc:
                    maxException = err
                    maxExcLoc = err.loc
            except IndexError:
                if len(instring) > maxExcLoc:
                    maxException = ParseException(instring, len(instring), e.errmsg, self)
                    maxExcLoc = len(instring)
            else:
                # save match among all matches, to retry longest to shortest
                matches.append((loc2, e))

        if matches:
            # re-evaluate all matches in descending order of length of match, in case attached actions
            # might change whether or how much they match of the input.
            matches.sort(key=itemgetter(0), reverse=True)

            if not doActions:
                # no further conditions or parse actions to change the selection of
                # alternative, so the first match will be the best match
                best_expr = matches[0][1]
                return best_expr._parse(instring, loc, doActions)

            longest = -1, None
            for loc1, expr1 in matches:
                if loc1 <= longest[0]:
                    # already have a longer match than this one will deliver, we are done
                    return longest

                try:
                    loc2, toks = expr1._parse(instring, loc, doActions)
                except ParseException as err:
                    err.__traceback__ = None
                    if err.loc > maxExcLoc:
                        maxException = err
                        maxExcLoc = err.loc
                else:
                    if loc2 >= loc1:
                        return loc2, toks
                    # didn't match as much as before
                    elif loc2 > longest[0]:
                        longest = loc2, toks

            if longest != (-1, None):
                return longest

        if maxException is not None:
            maxException.msg = self.errmsg
            raise maxException
        else:
            raise ParseException(instring, loc, "no defined alternatives to match", self)


    def __ixor__(self, other):
        return self.append(self.normalize(other))  # Or([self, other])

    def __str__(self):
        if hasattr(self, "name"):
            return self.name

        if self.strRepr is None:
            self.strRepr = "{" + " ^ ".join(_ustr(e) for e in self.exprs) + "}"

        return self.strRepr

    def checkRecursion(self, parseElementList):
        subRecCheckList = parseElementList[:] + [self]
        for e in self.exprs:
            e.checkRecursion(subRecCheckList)

    def _setResultsName(self, name, listAllMatches=False):
        if (not __compat__.collect_all_And_tokens
                and __diag__.warn_multiple_tokens_in_named_alternation):
            if any(isinstance(e, And) for e in self.exprs):
                warnings.warn("{0}: setting results name {1!r} on {2} expression "
                              "may only return a single token for an And alternative, "
                              "in future will return the full list of tokens".format(
                    "warn_multiple_tokens_in_named_alternation", name, type(self).__name__),
                    stacklevel=3)

        return super(Or, self)._setResultsName(name, listAllMatches)


class MatchFirst(ParseExpression):
    """Requires that at least one :class:`ParseExpression` is found. If
    two expressions match, the first one listed is the one that will
    match. May be constructed using the ``'|'`` operator.

    Example::

        # construct MatchFirst using '|' operator

        # watch the order of expressions to match
        number = Word(nums) | Combine(Word(nums) + '.' + Word(nums))
        print(number.searchString("123 3.1416 789")) #  Fail! -> [['123'], ['3'], ['1416'], ['789']]

        # put more selective expression first
        number = Combine(Word(nums) + '.' + Word(nums)) | Word(nums)
        print(number.searchString("123 3.1416 789")) #  Better -> [['123'], ['3.1416'], ['789']]
    """
    def __init__(self, exprs, savelist=False):
        super(MatchFirst, self).__init__(exprs, savelist)
        if self.exprs:
            self.mayReturnEmpty = any(e.mayReturnEmpty for e in self.exprs)
        else:
            self.mayReturnEmpty = True

    def streamline(self):
        super(MatchFirst, self).streamline()
        if __compat__.collect_all_And_tokens:
            self.saveAsList = any(e.saveAsList for e in self.exprs)
        return self

    def parseImpl(self, instring, loc, doActions=True):
        maxExcLoc = -1
        maxException = None
        for e in self.exprs:
            try:
                loc, ret = e._parse(instring, loc, doActions)
                return loc, ParseResults(self, ret.tokens_for_result)
            except ParseException as err:
                if err.loc > maxExcLoc:
                    maxException = err
                    maxExcLoc = err.loc
            except IndexError:
                if len(instring) > maxExcLoc:
                    maxException = ParseException(instring, len(instring), e.errmsg, self)
                    maxExcLoc = len(instring)

        # only got here if no expression matched, raise exception for match that made it the furthest
        else:
            if maxException is not None:
                maxException.msg = self.errmsg
                raise maxException
            else:
                raise ParseException(instring, loc, "no defined alternatives to match", self)

    def __ior__(self, other):
        return self.append(self.normalize(other))  # MatchFirst([self, other])

    def __str__(self):
        if hasattr(self, "name"):
            return self.name

        if self.strRepr is None:
            self.strRepr = "{" + " | ".join(_ustr(e) for e in self.exprs) + "}"

        return self.strRepr

    def checkRecursion(self, parseElementList):
        subRecCheckList = parseElementList[:] + [self]
        for e in self.exprs:
            e.checkRecursion(subRecCheckList)

    def _setResultsName(self, name, listAllMatches=False):
        if (not __compat__.collect_all_And_tokens
                and __diag__.warn_multiple_tokens_in_named_alternation):
            if any(isinstance(e, And) for e in self.exprs):
                warnings.warn("{0}: setting results name {1!r} on {2} expression "
                              "may only return a single token for an And alternative, "
                              "in future will return the full list of tokens".format(
                    "warn_multiple_tokens_in_named_alternation", name, type(self).__name__),
                    stacklevel=3)

        return super(MatchFirst, self)._setResultsName(name, listAllMatches)


class Each(ParseExpression):
    """Requires all given :class:`ParseExpression` s to be found, but in
    any order. Expressions may be separated by whitespace.

    May be constructed using the ``'&'`` operator.

    Example::

        color = oneOf("RED ORANGE YELLOW GREEN BLUE PURPLE BLACK WHITE BROWN")
        shape_type = oneOf("SQUARE CIRCLE TRIANGLE STAR HEXAGON OCTAGON")
        integer = Word(nums)
        shape_attr = "shape:" + shape_type("shape")
        posn_attr = "posn:" + Group(integer("x") + ',' + integer("y"))("posn")
        color_attr = "color:" + color("color")
        size_attr = "size:" + integer("size")

        # use Each (using operator '&') to accept attributes in any order
        # (shape and posn are required, color and size are optional)
        shape_spec = shape_attr & posn_attr & Optional(color_attr) & Optional(size_attr)

        test.runTests(shape_spec, '''
            shape: SQUARE color: BLACK posn: 100, 120
            shape: CIRCLE size: 50 color: BLUE posn: 50,80
            color:GREEN size:20 shape:TRIANGLE posn:20,40
            '''
            )

    prints::

        shape: SQUARE color: BLACK posn: 100, 120
        ['shape:', 'SQUARE', 'color:', 'BLACK', 'posn:', ['100', ',', '120']]
        - color: BLACK
        - posn: ['100', ',', '120']
          - x: 100
          - y: 120
        - shape: SQUARE


        shape: CIRCLE size: 50 color: BLUE posn: 50,80
        ['shape:', 'CIRCLE', 'size:', '50', 'color:', 'BLUE', 'posn:', ['50', ',', '80']]
        - color: BLUE
        - posn: ['50', ',', '80']
          - x: 50
          - y: 80
        - shape: CIRCLE
        - size: 50


        color: GREEN size: 20 shape: TRIANGLE posn: 20,40
        ['color:', 'GREEN', 'size:', '20', 'shape:', 'TRIANGLE', 'posn:', ['20', ',', '40']]
        - color: GREEN
        - posn: ['20', ',', '40']
          - x: 20
          - y: 40
        - shape: TRIANGLE
        - size: 20
    """
    def __init__(self, exprs, savelist=True):
        super(Each, self).__init__(exprs, savelist)
        self.mayReturnEmpty = all(e.mayReturnEmpty for e in self.exprs)
        self.skipWhitespace = True
        self.initExprGroups = True
        self.saveAsList = True

    def streamline(self):
        super(Each, self).streamline()
        self.mayReturnEmpty = all(e.mayReturnEmpty for e in self.exprs)
        return self

    def parseImpl(self, instring, loc, doActions=True):
        if self.initExprGroups:
            self.opt1map = dict((id(e.expr), e) for e in self.exprs if isinstance(e, Optional))
            opt1 = [e.expr for e in self.exprs if isinstance(e, Optional)]
            opt2 = [e for e in self.exprs if e.mayReturnEmpty and not isinstance(e, Optional)]
            self.optionals = opt1 + opt2
            self.multioptionals = [e.expr for e in self.exprs if isinstance(e, ZeroOrMore)]
            self.multirequired = [e.expr for e in self.exprs if isinstance(e, OneOrMore)]
            self.required = [e for e in self.exprs if not isinstance(e, (Optional, ZeroOrMore, OneOrMore))]
            self.required += self.multirequired
            self.initExprGroups = False
        tmpLoc = loc
        tmpReqd = self.required[:]
        tmpOpt  = self.optionals[:]
        matchOrder = []

        keepMatching = True
        while keepMatching:
            tmpExprs = tmpReqd + tmpOpt + self.multioptionals + self.multirequired
            failed = []
            for e in tmpExprs:
                try:
                    tmpLoc = e.tryParse(instring, tmpLoc)
                except ParseException:
                    failed.append(e)
                else:
                    matchOrder.append(self.opt1map.get(id(e), e))
                    if e in tmpReqd:
                        tmpReqd.remove(e)
                    elif e in tmpOpt:
                        tmpOpt.remove(e)
            if len(failed) == len(tmpExprs):
                keepMatching = False

        if tmpReqd:
            missing = ", ".join(_ustr(e) for e in tmpReqd)
            raise ParseException(instring, loc, "Missing one or more required elements (%s)" % missing)

        # add any unmatched Optionals, in case they have default values defined
        matchOrder += [e for e in self.exprs if isinstance(e, Optional) and e.expr in tmpOpt]

        resultlist = []
        for e in matchOrder:
            loc, results = e._parse(instring, loc, doActions)
            resultlist.append(results)

        finalResults = sum(resultlist, ParseResults.new_instance(self, []))
        return loc, finalResults

    def __str__(self):
        if hasattr(self, "name"):
            return self.name

        if self.strRepr is None:
            self.strRepr = "{" + " & ".join(_ustr(e) for e in self.exprs) + "}"

        return self.strRepr

    def checkRecursion(self, parseElementList):
        subRecCheckList = parseElementList[:] + [self]
        for e in self.exprs:
            e.checkRecursion(subRecCheckList)


# export
from pyparsing.parser import base

base.And = And
base.Or = Or
base.Each = Each
base.MatchFirst = MatchFirst

from pyparsing.parser import helpers
helpers.MatchFirst = MatchFirst
helpers.And = And
