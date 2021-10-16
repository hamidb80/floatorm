import macros, macroplus
import options, strutils, strformat, sequtils, tables

type
    DBColumnTypes {.pure.} = enum
        SCTint = "int"
        SCTtext = "string"
        SCTchar = "char"
        SCTfloat = "float"

    DBColumnFeatures {.pure.} = enum
        SCFNullable, SCFprimary

    DBTableFeatures = enum
        STFaddId


    Schema = Table[string, DBTable]

    DBTable = object
        name: string
        columns: seq[DBColumn]
        features: set[DBTableFeatures]

    DBColumn = object
        name: string
        `type`: DBColumnTypes
        typeLimit: int
        refrence: Option[tuple[tableName, fieldName: string]]
        features: set[DBColumnFeatures]

func `$`(features: set[DBColumnFeatures]): string =
    ([
      (SCFprimary in features , "PRIMARY KEY"),
      (SCFNullable notin features and SCFprimary notin features, "NOT NULL"),
    ]
    .filterIt(it[0]).mapit it[1]).join(" ")

func `$`(dbtype: DBColumnTypes): string =
    case dbtype:
    of SCTint: "INTEGER"
    of SCTtext: "TEXT"
    of SCTchar: "CHAR"
    of SCTfloat: "REAL"

func getColumnType(c: DBColumn): string=
  result = $ c.`type`

  if c.typelimit != 0:
    result &= fmt"({c.typelimit})"

func `$`(t: DBTable): string =
    fmt"TABLE {t.name}(" & "\n" & (
        t.columns.mapIt indent(fmt"{it.name} {getColumnType it} {it.features}", 4)
    ).join(",\n") & "\n);"

func columnGen(rawColumn: NimNode): DBColumn =
    let columnName = rawColumn[CommandIdent].strVal
    var params = rawColumn[CommandBody]

    if params[0].kind == nnkCommand: # for columns with featues
        params = params[0]

    var `type` = params[0]
    result = DBColumn(name: columnName)

    # FIXME not working with Option[char[200]]
    if `type`.kind == nnkBracketExpr:
        if `type`[BracketExprIdent].strVal == "Option": # Option[string]
            result.features.incl SCFNullable
            `type` = `type`[1]

        else: # string[value] | int[ref anotherTable.field]
            let
                args = `type`[BracketExprParams]
                firstArg = args[0]
            `type` = `type`[BracketExprIdent]

            if firstArg.kind == nnkRefTy:
                doassert firstArg[0].kind == nnkDotExpr

                let
                    refTable = firstArg[0][0].strval
                    refField = firstArg[0][1].strval

            elif firstarg.allIt it.kind in [nnkIntLit, nnkStrLit]:
                result.typeLimit = args[0].intVal.int

            else:
                error "invalid type options"

    result.`type` = parseEnum[DBColumnTypes](`type`.strVal)

    if params.len == 2:
        for feature in params[1]:
            result.features.incl:
                case feature.strval:
                of "primary": SCFprimary
                else: raise newException(ValueError, "column feature is not defined")


proc tableGen(rawTable: NimNode): DBTable =
    doAssert rawTable[CommandIdent].strVal == "Table", "Entity is not Valid"
    let tableName = rawTable[1].strVal

    result = DBTable(name: tableName)
    result.columns = rawTable[CommandBody].mapIt it.columnGen


proc schemaGen(args, body: NimNode): Schema =
    for rawTable in body:
        let table = tableGen(rawTable)
        result[table.name] = table

macro Blueprint(features, body) =
    echo treeRepr body

    let schema = schemaGen(features, body)

    for (name, table) in schema.pairs:
        echo "CREATE ", table


Blueprint [autoId]:
    Table test1:
        id: int {primary} 
        table_id: int[ref another.id]
        name: char[255]

    # Table members:
    #     id: int {primary}
    #     name: string[255]

    # Table part:
    #     id: int {primary}
    #     name: string

    # Table quiz:
    #     id: int {primary}
    #     member_id: int[ref members.id]
    #     name: string[255]
    #     part_id: int[ref part.id]

    # Table question:
    #     id: int {primary}
    #     quiz_id: int[ref quiz.id]
    #     answer: int

    # Table record:
    #     id: int {primary}
    #     member_id: int[ref members.id] {update: restrict, delete: restric}
    #     date: DateTime {auto}

    # Table test:
    #     field: Option[string]
