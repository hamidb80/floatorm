import macros, macroplus
import options, strutils, strformat, sequtils, tables

type
    DBColumnTypes {.pure.} = enum
        SCTint = "int"
        SCTtext = "string"
        SCTchar = "char"
        SCTfloat = "float"

    DBColumnFeatures {.pure.} = enum
        SCFNullable

    DBTableFeatures = enum
        STFaddId


    Schema* = OrderedTable[string, DBTable]

    DBTable = object
        name: string
        columns: seq[DBColumn]
        features: set[DBTableFeatures]
        primaryKeys: seq[string]
        refKeys: seq[tuple[`from`, to: tuple[table, field: string]]]

    DBColumn = object
        name: string
        `type`: DBColumnTypes
        typeLimit: int
        refrence: Option[tuple[tableName, fieldName: string]]
        features: set[DBColumnFeatures]

func `$`(features: set[DBColumnFeatures]): string =
    ([
      (SCFNullable notin features, "NOT NULL"),
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

const indentVal = 4

func `$`(t: DBTable): string =
    result = fmt"TABLE {t.name}(" & "\n" & (
        t.columns.mapIt indent(fmt"{it.name} {getColumnType it} {it.features}", indentVal)
    ).join(",\n") 

    if t.primaryKeys.len != 0:
        result &= ",\n" & ( 
            ("PRIMARY KEY (" & t.primaryKeys.join(", ") & ")").indent(indentVal)
        )
        
    result &= "\n);"

func parseColumnType(c: var DBColumn, `type`: NimNode): DBColumnTypes=
    var mytype = `type`

    if mytype.kind == nnkBracketExpr:
        if mytype[BracketExprIdent].strVal == "Option": # Option[string]
            c.features.incl SCFNullable
            mytype = mytype[1]
            return parseColumnType(c, mytype)
        
        let
            args = mytype[BracketExprParams]
            firstArg = args[0]

        mytype = mytype[BracketExprIdent]

        if firstArg.kind == nnkRefTy:
            doassert firstArg[0].kind == nnkDotExpr

            let
                refTable = firstArg[0][0].strval
                refField = firstArg[0][1].strval

        elif firstarg.allIt it.kind in [nnkIntLit, nnkStrLit]:
            c.typeLimit = args[0].intVal.int

        else:
            error "invalid type options"

    return parseEnum[DBColumnTypes](mytype.strVal)

func addFeatures(t: var DBTable, c: DBColumn, featuresExpr: NimNode)=
    for feature in featuresExpr:
        case feature.strval:
        of "primary": 
            t.primaryKeys.add c.name
        else:
            raise newException(ValueError, "column feature is not defined")


func columnGen(table: var DBTable, rawColumn: NimNode): DBColumn =
    let columnName = rawColumn[CommandIdent].strVal
    var params = rawColumn[CommandBody]

    if params[0].kind == nnkCommand: # for columns with featues
        params = params[0]

    result = DBColumn(name: columnName)
    result.`type` = parseColumnType(result, params[0])

    if params.len == 2:
        addFeatures table, result, params[1]
            

proc tableGen(rawTable: NimNode): DBTable =
    doAssert rawTable[CommandIdent].strVal == "Table", "Entity is not Valid"
    let tableName = rawTable[1].strVal

    result = DBTable(name: tableName)
    result.columns = rawTable[CommandBody].mapIt columnGen(result, it)


proc schemaGen(args, body: NimNode): Schema =
    for rawTable in body:
        let table = tableGen(rawTable)
        result[table.name] = table

macro Blueprint*(features, body) =
    echo treeRepr body

    let schema = schemaGen(features, body)

    for (name, table) in schema.pairs:
        echo "CREATE ", table
