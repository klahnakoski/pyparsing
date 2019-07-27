# encoding: utf-8
import sys

from pyparsing import ParserElement

from pyparsing.exceptions import ParseBaseException, ParseFatalException
from pyparsing.parser.base import quotedString, replaceWith
from pyparsing.parser.results import ParseResults
from pyparsing.tokens import Literal
from pyparsing.utils import basestring, col, line


def runTests(self, tests, parseAll=True, comment='#',
             fullDump=True, printResults=True, failureTests=False, postParse=None,
             file=None):
    """
    Execute the parse expression on a series of test strings, showing each
    test, the parsed results or where the parse failed. Quick and easy way to
    run a parse expression against a list of sample strings.

    Parameters:
     - tests - a list of separate test strings, or a multiline string of test strings
     - parseAll - (default= ``True``) - flag to pass to :class:`parseString` when running tests
     - comment - (default= ``'#'``) - expression for indicating embedded comments in the test
          string; pass None to disable comment filtering
     - fullDump - (default= ``True``) - dump results as list followed by results names in nested outline;
          if False, only dump nested list
     - printResults - (default= ``True``) prints test output to stdout
     - failureTests - (default= ``False``) indicates if these tests are expected to fail parsing
     - postParse - (default= ``None``) optional callback for successful parse results; called as
          `fn(test_string, parse_results)` and returns a string to be added to the test output
     - file - (default=``None``) optional file-like object to which test output will be written;
          if None, will default to ``sys.stdout``

    Returns: a (success, results) tuple, where success indicates that all tests succeeded
    (or failed if ``failureTests`` is True), and the results contain a list of lines of each
    test's output

    Example::

        number_expr = pyparsing_common.number.copy()

        result = test.runTests(number_expr, '''
            # unsigned integer
            100
            # negative integer
            -100
            # float with scientific notation
            6.02e23
            # integer with scientific notation
            1e-12
            ''')
        print("Success" if result[0] else "Failed!")

        result = test.runTests(number_expr, '''
            # stray character
            100Z
            # missing leading digit before '.'
            -.100
            # too many '.'
            3.14.159
            ''', failureTests=True)
        print("Success" if result[0] else "Failed!")

    prints::

        # unsigned integer
        100
        [100]

        # negative integer
        -100
        [-100]

        # float with scientific notation
        6.02e23
        [6.02e+23]

        # integer with scientific notation
        1e-12
        [1e-12]

        Success

        # stray character
        100Z
           ^
        FAIL: Expected end of text (at char 3), (line:1, col:4)

        # missing leading digit before '.'
        -.100
        ^
        FAIL: Expected {real number with scientific notation | real number | signed integer} (at char 0), (line:1, col:1)

        # too many '.'
        3.14.159
            ^
        FAIL: Expected end of text (at char 4), (line:1, col:5)

        Success

    Each test string must be on a single line. If you want to test a string that spans multiple
    lines, create a test like this::

        expr.runTest(r"this is a test\\n of strings that spans \\n 3 lines")

    (Note that this is a raw string literal, you must include the leading 'r'.)
    """
    if isinstance(tests, basestring):
        tests = list(map(str.strip, tests.rstrip().splitlines()))
    if isinstance(comment, basestring):
        comment = Literal(comment)
    if file is None:
        file = sys.stdout
    print_ = file.write

    allResults = []
    comments = []
    success = True
    NL = Literal(r'\n').addParseAction(replaceWith('\n')).ignore(quotedString)
    BOM = u'\ufeff'
    for t in tests:
        if comment is not None and comment.matches(t, False) or comments and not t:
            comments.append(t)
            continue
        if not t:
            continue
        out = ['\n'.join(comments), t]
        comments = []
        try:
            # convert newline marks to actual newlines, and strip leading BOM if present
            t = NL.transformString(t.lstrip(BOM))
            result = self.parseString(t, parseAll=parseAll)
        except ParseBaseException as pe:
            fatal = "(FATAL)" if isinstance(pe, ParseFatalException) else ""
            if '\n' in t:
                out.append(line(pe.loc, t))
                out.append(' ' * (col(pe.loc, t) - 1) + '^' + fatal)
            else:
                out.append(' ' * pe.loc + '^' + fatal)
            out.append("FAIL: " + str(pe))
            success = success and failureTests
            result = pe
        except Exception as exc:
            out.append("FAIL-EXCEPTION: " + str(exc))
            success = success and failureTests
            result = exc
        else:
            success = success and not failureTests
            if postParse is not None:
                try:
                    pp_value = postParse(t, result)
                    if pp_value is not None:
                        if isinstance(pp_value, ParseResults):
                            out.append(pp_value.dump())
                        else:
                            out.append(str(pp_value))
                    else:
                        out.append(result.dump())
                except Exception as e:
                    out.append(result.dump(full=fullDump))
                    out.append("{0} failed: {1}: {2}".format(postParse.__name__, type(e).__name__, e))
            else:
                out.append(result.dump(full=fullDump))

        if printResults:
            if fullDump:
                out.append('')
            print_('\n'.join(out))

        allResults.append((t, result))

    return success, allResults


ParserElement.runTests = runTests
