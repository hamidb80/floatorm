import unittest, strutils, sequtils, options
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

    test "nothing":
        Blueprint []:
            Table test:
                id: int

        discard test()

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

        test "MULTI PRIMARY KEYS":
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
                id: int
                name: char[256]
                price: float
                bio: string

        check:
            model.id is int
            model.name is string
            model.price is float
            model.bio is string

    test "optional types":
        Blueprint []:
            Table model:
                id: Option[int]
                name: Option[char[256]]

        check:
            model.id is Option[int]
            model.name is Option[string]


