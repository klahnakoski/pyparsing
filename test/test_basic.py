# encoding: utf-8
from unittest import TestCase
import uuid

from pyparsing import CaselessLiteral, Group, Word, alphanums, alphas, delimitedList, pyparsing_common, tokenMap, upcaseTokens
from test import runTests


class TestBasic(TestCase):
    def test(self):
        selectToken = CaselessLiteral("select")
        fromToken = CaselessLiteral("from")

        ident = Word(alphas, alphanums + "_$")

        columnName = delimitedList(ident, ".", combine=True).setParseAction(upcaseTokens)
        columnNameList = Group(delimitedList(columnName)).setName("columns")
        columnSpec = ('*' | columnNameList)

        tableName = delimitedList(ident, ".", combine=True).setParseAction(upcaseTokens)
        tableNameList = Group(delimitedList(tableName)).setName("tables")

        simpleSQL = selectToken("command") + columnSpec("columns") + fromToken + tableNameList("tables")

        # demo runTests method, including embedded comments in test string
        runTests(simpleSQL, """
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

        runTests(pyparsing_common.number, """
            100
            -100
            +100
            3.14159
            6.02e23
            1e-12
            """)

        # any int or real number, returned as float
        runTests(pyparsing_common.fnumber, """
            100
            -100
            +100
            3.14159
            6.02e23
            1e-12
            """)

        runTests(pyparsing_common.hex_integer, """
            100
            FF
            """)

        pyparsing_common.uuid.setParseAction(tokenMap(uuid.UUID))
        runTests(pyparsing_common.uuid, """
            12345678-1234-5678-1234-567812345678
            """)
