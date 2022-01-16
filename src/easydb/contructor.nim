import std/[options, strutils, strformat, sequtils, tables]
import macros, macroplus

type
    DBColumnTypes = enum
        ctInt, ctText, ctChar, ctFloat

    DBColumnFeatures = enum
        cfNullable, cfUnique

    SchemaOptions = object
        queryHolder: NimNode # variable to save init query
        prefix: string       # table name prefix
        postfix: string      # table name postfix

    Schema* = OrderedTable[string, DBTable]

    DBTable = object
        name: string
        columns: seq[DBColumn]
        primaryKeys: seq[string]
        uniqueColumns: seq[string]
        refKeys: seq[tuple[`from`: string, to: tuple[table, column: string]]]
        indexes: seq[DBMultiColumnIndex]

    DBMultiColumnIndex = object
        name: string
        columns: seq[string]

    DBColumn = object
        name: string
        `type`: DBColumnTypes
        typeArg: Option[int]
        reference: Option[tuple[tableName, fieldName: string]]
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
        of ctInt: "int64"
        of ctText: "string"
        of ctChar: "string"
        of ctFloat: "float64"

func nimtype2sqlite(`type`: string): DBColumnTypes =
    case `type`:
    of "int", "int8", "int32", "int64": ctInt
    of "string": ctText
    of "char": ctChar
    of "float", "float32", "float64": ctFloat
    else:
        raise newException(ValueError, "the type is not supported: " & `type`)

func `$`(features: set[DBColumnFeatures]): string =
    ([
      (cfNullable notin features, "NOT NULL"),
    ]
    .filterIt(it[0]).mapit it[1]).join(" ")

func `$`(dbtype: DBColumnTypes): string =
    case dbtype:
    of ctInt: "INTEGER"
    of ctText: "TEXT"
    of ctChar: "CHAR"
    of ctFloat: "REAL"

func getColumnType(c: DBColumn): string =
    result = $ c.`type`

    if issome c.typeArg:
        result &= fmt"({c.typeArg.get})"

func getDefaultValIfExists(defaultVal: Option[DBDefaultValue]): string =
    if not issome defaultVal:
        return

    "DEFAULT " & (
        case defaultVal.get.kind:
        of vkInt: $ defaultval.get.intval
        of vkFloat: $ defaultval.get.floatval
        of vkString: "'" & defaultval.get.strval & "'"
        of vkBool: $ defaultval.get.boolval
        of vkNil: "NULL"
    )

func toSql(t: DBTable, indentVal = 4): seq[string] =
    result.add fmt"CREATE TABLE {t.name}(" & "\n" & (
        t.columns.mapIt [
            it.name,
            getColumnType it,
            $ it.features,
            getDefaultValIfExists it.defaultValue
        ].filterIt(it != "").join(" ").indent(indentVal)
    ).join ",\n"

    if t.primaryKeys.len != 0:
        result[0] &= ",\n" &
            ("PRIMARY KEY (" & t.primaryKeys.join(", ") & ")").indent(indentVal)

    if t.uniqueColumns.len != 0:
        result[0] &= ",\n" &
            ("UNIQUE (" & t.uniqueColumns.join(", ") & ")").indent(indentVal)

    if t.refkeys.len != 0:
        result[0] &= ",\n" & (
            t.refkeys.mapIt (fmt"FOREIGN KEY ({it.`from`}) REFERENCES {it.to.table} ({it.to.column})").indent(indentVal)
        ).join ",\n"

    result[0] &= "\n);"

    for index in t.indexes:
        let fields = index.columns.join ", "
        result.add fmt"CREATE INDEX {index.name} ON {t.name}({fields});"

# ---------------------------

func resolveColumnType(
    t: var DBTable, c: var DBColumn, `type`: NimNode
): DBColumnTypes =
    var mytype = `type`

    if mytype.kind == nnkBracketExpr:
        if mytype[BracketExprIdent].strVal == "Option":
            c.features.incl cfNullable
            mytype = mytype[1]
            return resolveColumnType(t, c, mytype)
        
        else:
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

            elif firstarg.kind in {nnkIntLit, nnkStrLit}:
                c.typeArg = some firstArg.intVal.int

            else:
                error "invalid type options: " & repr mytype

    return nimtype2sqlite(mytype.strVal)

func addFeatures(t: var DBTable, c: var DBColumn, featuresExpr: NimNode) =
    template notFound =
        raise newException(ValueError, "column feature is not defined: " & $featuresExpr)

    for feature in featuresExpr:
        case feature.kind:
        of nnkExprColonExpr:
            let nval = feature[ColonExprrightSide]

            case feature[ColonExprLeftSide].strval.normalize:
            of "default":
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
                        raise newException(ValueError,
                                "invalid default value: " & $nval.kind)

            of "index":
                t.indexes.add DBMultiColumnIndex(name: nval.strVal, columns: @[c.name])

            else: notFound

        of nnkIdent:
            case feature.strval.normalize:
            of "primary":
                t.primaryKeys.add c.name
            of "unique":
                t.uniqueColumns.add c.name
            of "index":
                t.indexes.add DBMultiColumnIndex(name: fmt"{c.name}_index", columns: @[c.name])

            else: notFound
        else:
            error "feature nim node kind is not acceptable: " & $feature.kind

func columnGen(table: var DBTable, rawColumn: NimNode): DBColumn =
    result.name = rawColumn[CommandIdent].strVal
    var params = rawColumn[CommandBody]

    if params[0].kind == nnkPragmaExpr: # for columns with featues
        params = params[0]

    result.`type` = resolveColumnType(table, result, params[0])

    if params.len == 2: # with pragme
        addFeatures table, result, params[1]

func toStrSeq(node: NimNode): seq[string] =
    doAssert node.kind == nnkBracket
    node.mapIt $it

func indexGen(entity: NimNode): DBMultiColumnIndex =
    if entity[1].kind == nnkInfix: # as
        result.name = entity[1][2].strVal
        result.columns = entity[1][1].toStrSeq
    else:
        result.columns = entity[1].toStrSeq
        result.name = result.columns.join("_") & "_index"

func tableGen(rawTable: NimNode): DBTable =
    doAssert rawTable[CommandIdent].strVal == "Table", "Entity is not Valid"
    let tableName = rawTable[1].strVal

    result = DBTable(name: tableName)

    for entity in rawTable[CommandBody]:
        if entity[0].strval == "Index":
            result.indexes.add indexGen entity
        else:
            result.columns.add columnGen(result, entity)

func schema2objectDefs(sch: Schema): NimNode =
    result = newStmtList()

    for (tableName, table) in sch.pairs:
        let tableIdent = ident capitalizeAscii tableName
        var objDef =
            when defined easydbtest:
                quote:
                    type `tableIdent` = object
            else:
                quote:
                    type `tableIdent`* = object


        objdef[0][^1][^1] = newNimNode(nnkRecList)

        for col in table.columns:
            let maybeType = columnType2nimIdent(col.`type`)

            objdef[0][^1][^1].add newIdentDefs(
                newNimNode(nnkPostfix).add(ident "*").add(ident col.name),

                if cfNullable in col.features:
                    newNimNode(nnkbracketExpr).add(bindsym "Option", maybeType)
                else:
                    maybeType
            )

        result.add objdef

func schemaGen(options: SchemaOptions, body: NimNode): Schema =
    for rawTable in body:
        let table = tableGen(rawTable)
        let tablename = table.name

        result[options.prefix & tablename & options.postfix] = table

    # debugEcho "-------------------\n", result

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
            error "undefined option key: " & field

macro Blueprint*(options, body) =
    let
        ro = resolveSchemeOptions options
        schema = schemaGen(ro, body)

    result = schema2objectDefs schema

    if (let path = ro.queryHolder; path != nil):
        var queryList: seq[string]
        for table in schema.values:
            queryList.add toSql table

        result.add quote do:
            `path` = @`queryList`

    # echo result.repr
