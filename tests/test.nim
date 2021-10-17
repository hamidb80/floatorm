import unittest, macros, os
import easydb

suite "compiles":
    test "":
        Blueprint [postfix: "Model"]:
            Table members:
                id: int {.primary.}
                name: string[255]

            Table part:
                id: int {.primary.}
                name: string

            Table quiz:
                id: int {.primary.}
                member_id: int[ref members.id]
                name: char[255]
                part_id: int[ref part.id]

            Table question:
                id: int {.primary.}
                quiz_id: int[ref quiz.id]
                answer: int

            Table record:
                id: int {.primary.}
                answerts: Option[char[400]] {.default: 100.}
                question_order: char[400] {.primary.}
                member_id: int[ref members.id] #TODO {on_update: restrict, onDelete: restric}
                date: char[400] {.primary, default: "val".}

            Table test:
                field: Option[char[255]]


template createTable(path = "temp.sql"): untyped =
    Blueprint [savePath: path, postfix: "t1"]:
        Table members:
            id: int
            name: char

    let query = readfile path
    removefile path

    query

suite "table creation":
    setup:
        echo "run before each test"
  
    teardown:
        echo "run after each test"
    
    test "1":
        echo createTable()
