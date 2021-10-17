import unittest, strutils, sequtils
import easydb

suite "nameing options":
    test "prefix & postfix":
        Blueprint [prefix: "zz", postfix: "Model"]:
            Table test:
                id: int

        discard zztestModel()

    test "prefix":
        Blueprint [prefix: "zz"]:
            Table test:
                id: int

        discard zztest()

    test "postfix":
        Blueprint [postfix: "zz"]:
            Table test:
                id: int

        discard testzz()

func createTable(tableName: string, rows: openArray[string]): string =
    "CREATE TABLE test(\n" &
    rows.mapIt(it.indent 4).join(",\n") &
    "\n);"

suite "table creation":
    var query: string

    test "simple typed columns":
        Blueprint [queryHolder: query]:
            Table test:
                id: int
                name: char[256]

        check query == "test".createTable [
            "id INTEGER NOT NULL",
            "name CHAR(256) NOT NULL"
        ]


    suite "columns with options":
        test "PRIMARY":
            Blueprint [queryHolder: query]:
                Table test:
                    id: int {.primary.}

            check query == "test".createTable [
                "id INTEGER NOT NULL",
                "PRIMARY KEY (id)"
            ]

        test "DEFAULT VALUE":
            Blueprint [queryHolder: query]:
                Table test:
                    id: int {.default: 10.}

            check query == "test".createTable [
                "id INTEGER NOT NULL DEFAULT 10"
            ]

    test "OPTIONAL":
        Blueprint [queryHolder: query]:
            Table test:
                id: Option[int]
                name: Option[char[256]]

        check query == "test".createTable [
            "id INTEGER",
            "name CHAR(256)"
        ]


    test "RELATION":
        Blueprint [queryHolder: query]:
            Table test:
                out_id: int[ref other.id]

        check query == "test".createTable [
            "out_id INTEGER NOT NULL",
            "FOREIGN KEY (out_id) REFERENCES other (id)"
        ]
