import unittest, strutils, sequtils, options
import easydb

suite "nameing options":
    test "prefix & postfix":
        var query: seq[string]

        Blueprint [prefix: "zz", postfix: "Model", queryHolder: query]:
            Table my_table:
                id: int

        discard ZzMyTableModel()
        check: # NOTE: prefix and postfix do not have any effects on actual table name
            "my_table" in query.join

    test "prefix":
        Blueprint [prefix: "zz"]:
            Table test:
                id: int

        discard Zztest()

    test "postfix":
        Blueprint [postfix: "zz"]:
            Table test:
                id: int

        discard Testzz()

    test "nothing":
        Blueprint []:
            Table test:
                id: int

        discard Test()

func createTable(tableName: string, rows: openArray[string]): string =
    "CREATE TABLE test(\n" &
    rows.mapIt(it.indent 4).join(",\n") &
    "\n);"

suite "table creation":
    var query: seq[string]

    test "simple typed columns":
        Blueprint [queryHolder: query]:
            Table test:
                id: int
                name: char[256]

        check query.join == "test".createTable [
            "id INTEGER NOT NULL",
            "name CHAR(256) NOT NULL"
        ]


    suite "columns with options":
        test "PRIMARY":
            Blueprint [queryHolder: query]:
                Table test:
                    id: int {.primary.}

            check query.join == "test".createTable [
                "id INTEGER NOT NULL",
                "PRIMARY KEY (id)"
            ]

        test "MULTI PRIMARY KEYS":
            Blueprint [queryHolder: query]:
                Table test:
                    id: int {.primary.}
                    number: int {.primary.}

            check query.join == "test".createTable [
                "id INTEGER NOT NULL",
                "number INTEGER NOT NULL",
                "PRIMARY KEY (id, number)"
            ]

        test "DEFAULT VALUE":
            Blueprint [queryHolder: query]:
                Table test:
                    id: int {.default: 10.}

            check query.join == "test".createTable [
                "id INTEGER NOT NULL DEFAULT 10"
            ]

    test "OPTIONAL":
        Blueprint [queryHolder: query]:
            Table test:
                id: Option[int]
                name: Option[char[256]]

        check query.join == "test".createTable [
            "id INTEGER",
            "name CHAR(256)"
        ]


    test "RELATION":
        Blueprint [queryHolder: query]:
            Table test:
                out_id: int[ref other.id]

        check query.join == "test".createTable [
            "out_id INTEGER NOT NULL",
            "FOREIGN KEY (out_id) REFERENCES other (id)"
        ]

    test "INDEX :: single":
        Blueprint [queryHolder: query]:
            Table mytbl:
                mycol: int {.index: "myidx".}

        check query[1] == "CREATE INDEX myidx ON mytbl(mycol);"


suite "correspoding object defenition":
    test "simple types":
        Blueprint []:
            Table model:
                id: int
                name: char[256]
                price: float
                bio: string

        check:
            Model.id is int
            Model.name is string
            Model.price is float
            Model.bio is string

    test "optional types":
        Blueprint []:
            Table model:
                id: Option[int]
                name: Option[char[256]]

        check:
            Model.id is Option[int]
            Model.name is Option[string]
