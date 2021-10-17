import options, strutils, strformat, sequtils, tables
import macros, macroplus, sugar

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

    SchemaOptions = object
        queryHolder: NimNode # variable to save init query
        prefix: string    # table name prefix
        postfix: string   # table name postfix

    Schema* = OrderedTable[string, DBTable]

    DBTable = object
        name: string
        columns: seq[DBColumn]
        features: set[DBTableFeatures]
        primaryKeys: seq[string]
        refKeys: seq[tuple[`from`: string, to: tuple[table, column: string]]]

    DBColumn = object
        name: string
        `type`: DBColumnTypes
        typeArg: int
        refrence: Option[tuple[tableName, fieldName: string]]
        features: set[DBColumnFeatures]
        defaultValue: Option[DBDefaultValue]

    DefaltValueKind = enum
        vkInt, vkFloat, vkString, vkNil, vkBool

    DBDefaultValue = object
        case kind: DefaltValueKind
        of vkInt: intval: int
        of vkFloat: floatval: float
        of vkString: strval: string
        of vkNil: nil
        of vkBool: boolval: bool

func columnType2nimIdent(ct: DBColumnTypes): NimNode =
    ident:
        case ct:
        of SCTint: "int"
        of SCTtext: "string"
        of SCTchar: "string"
        of SCTfloat: "float"

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

func getColumnType(c: DBColumn): string =
    result = $ c.`type`

    if c.typeArg != 0:
        result &= fmt"({c.typeArg})"

func getDefaultValIfExists(defaultVal: Option[DBDefaultValue]): string =
    if not issome defaultVal:
        return

    "DEFAULT " & (
        case defaultVal.get.kind:
        of vkInt: $ defaultval.get.intval
        of vkFloat: $ defaultval.get.floatval
        of vkString: '"' & defaultval.get.strval & '"'
        of vkBool: $ defaultval.get.boolval
        of vkNil: "NULL"
    )

func `$`(t: DBTable, indentVal = 4): string =
    result = fmt"TABLE {t.name}(" & "\n" & (
        t.columns.mapIt [
            it.name,
            getColumnType it,
            $ it.features,
            getDefaultValIfExists it.defaultValue
        ].filterIt(it != "").join(" ").indent(indentVal)
    ).join ",\n"

    if t.primaryKeys.len != 0:
        result &= ",\n" & (
            ("PRIMARY KEY (" & t.primaryKeys.join(", ") & ")").indent(indentVal)
        )

    if t.refkeys.len != 0:
        result &= ",\n" & (
            t.refkeys.mapIt (fmt"FOREIGN KEY ({it.`from`}) REFERENCES {it.to.table} ({it.to.column})").indent(indentVal)
        ).join ",\n"


    result &= "\n);"

# ---------------------------

func resolveColumnType(t: var DBTable, c: var DBColumn,
        `type`: NimNode): DBColumnTypes =
    var mytype = `type`

    if mytype.kind == nnkBracketExpr:
        if mytype[BracketExprIdent].strVal == "Option":
            c.features.incl SCFNullable
            mytype = mytype[1]
            return resolveColumnType(t, c, mytype)

        let
            args = mytype[BracketExprParams]
            firstArg = args[0]

        mytype = mytype[BracketExprIdent]

        if firstArg.kind == nnkRefTy:
            doassert firstArg[0].kind == nnkDotExpr

            let
                refTable = firstArg[0][0].strval
                refColumn = firstArg[0][1].strval

            t.refkeys.add (c.name, (refTable, refColumn))

        elif firstarg.allIt it.kind in [nnkIntLit, nnkStrLit]:
            c.typeArg = args[0].intVal.int

        else:
            error "invalid type options"

    return parseEnum[DBColumnTypes](mytype.strVal)

func addFeatures(t: var DBTable, c: var DBColumn, featuresExpr: NimNode) =
    template notFound =
        raise newException(ValueError, "column feature is not defined")

    for feature in featuresExpr:
        case feature.kind:
        of nnkExprColonExpr:
            case feature[ColonExprLeftSide].strval.normalize:
            of "default":
                let nval = feature[ColonExprrightSide]
                c.defaultValue = some:
                    case nval.kind:
                    of nnkIntLit: DBDefaultValue(kind: vkInt,
                            intval: nval.intval.int)
                    of nnkFloatLit: DBDefaultValue(kind: vkFloat,
                            floatVal: nval.floatVal.float)
                    of nnkStrLit: DBDefaultValue(kind: vkString,
                            strval: nval.strVal)
                    of nnkNilLit: DBDefaultValue(kind: vkNil)
                    of nnkIdent: DBDefaultValue(kind: vkBool,
                            boolVal: parseBool nval.strVal)
                    else:
                        raise newException(ValueError, "invalid default value")

            else: notFound

        of nnkIdent:
            case feature.strval.normalize:
            of "primary":
                t.primaryKeys.add c.name

            else: notFound

        else:
            error "feature nim node kind is not acceptable"

func columnGen(table: var DBTable, rawColumn: NimNode): DBColumn =
    let columnName = rawColumn[CommandIdent].strVal
    var params = rawColumn[CommandBody]

    if params[0].kind == nnkPragmaExpr: # for columns with featues
        params = params[0]

    result = DBColumn(name: columnName)
    result.`type` = resolveColumnType(table, result, params[0])

    if params.len == 2:
        addFeatures table, result, params[1]

func tableGen(rawTable: NimNode): DBTable =
    doAssert rawTable[CommandIdent].strVal == "Table", "Entity is not Valid"
    let tableName = rawTable[1].strVal

    result = DBTable(name: tableName)
    result.columns = rawTable[CommandBody].mapIt columnGen(result, it)

func schema2objectDefs(sch: Schema): NimNode =
    result = newStmtList()

    for (tableName, table) in sch.pairs:
        let tableIdent = ident tableName
        var objDef = 
            when defined test:
                quote:
                    type `tableIdent` = object
            else:
                quote:
                    type `tableIdent`* = object


        objdef[0][^1][^1] = newNimNode(nnkRecList)

        for col in table.columns:
            let maybeType = columnType2nimIdent(col.`type`)

            objdef[0][^1][^1].add newIdentDefs(
                ident(col.name),

                if SCFNullable in col.features:
                    newNimNode(nnkbracketExpr).add(bindsym "Option", maybeType)
                else:
                    maybeType
            )

        result.add objdef

func schemaGen(options: SchemaOptions, body: NimNode): Schema =
    for rawTable in body:
        let table = tableGen(rawTable)
        result[options.prefix & table.name & options.postfix] = table

func resolveSchemeOptions(options: NimNode): SchemaOptions =
    doAssert options.kind == nnkBracket

    for pair in options:
        doAssert pair.kind == nnkExprColonExpr, "undefined option type | it must be like prop: value"
        let
            field = pair[0].strval
            value = pair[1]

        case field.normalize:
        of "queryholder": result.queryHolder = value
        of "prefix": result.prefix = value.strval
        of "postfix": result.postfix = value.strval
        else:
            error "undefined option key"

macro Blueprint*(options, body) =
    let 
        resolvedOptions = resolveSchemeOptions options
        schema = schemaGen(resolvedOptions, body)

    result = schema2objectDefs schema
    
    if resolvedOptions.queryHolder != nil:
        let tablesQuery = collect newseq:
            for (name, table) in schema.pairs:
                "CREATE " & $table

        let 
            path = resolvedOptions.queryHolder
            query = tablesQuery.join "\n"
        
        result.add quote do:
            `path` = `query`

    # echo result.repr
