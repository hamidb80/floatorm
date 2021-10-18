import unittest, strutils, sequtils, options
import easydb

suite "nameing options":
    test "prefix & postfix":
        var query: string

        Blueprint [prefix: "zz", postfix: "Model", queryHolder: query]:
            Table my_table:
                id: int

        discard ZzMyTableModel()
        check: # NOTE: prefix and postfix do not have any effects on actual table name
            "my_table" in query 

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
        test "PRIMARY KEYS":
            Blueprint [queryHolder: query]:
                Table test:
                    id: int {.primary.}
                    number: int {.primary.}

            check query == "test".createTable [
                "id INTEGER NOT NULL",
                "number INTEGER NOT NULL",
                "PRIMARY KEY (id, number)"
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

    test "UNIQUE":
        Blueprint [queryHolder: query]:
            Table test:
                id: int {.unique.}
                name: string {.unique.}

        check query == "test".createTable [
            "id INTEGER NOT NULL",
            "name TEXT NOT NULL",
            "UNIQUE (id, name)"
        ]


    test "RELATION":
        Blueprint [queryHolder: query]:
            Table test:
                out_id: int[ref other.id]

        check query == "test".createTable [
            "out_id INTEGER NOT NULL",
            "FOREIGN KEY (out_id) REFERENCES other (id)"
        ]

suite "correspoding object defenition":
    test "simple types":
        Blueprint []:
            Table model:
                id: int64
                name: char[256]
                price: float64
                bio: string

        check:
            Model.id is int64
            Model.name is string
            Model.price is float64
            Model.bio is string

    test "optional types":
        Blueprint []:
            Table model:
                id: Option[int64]
                name: Option[char[256]]

        check:
            Model.id is Option[int64]
            Model.name is Option[string]


