import db_sqlite {.all.}, sequtils

template dbworks*(path: string, body): untyped =
    ## opens a db connection then closes; it wont close if a error occurs 
    runnableExamples:
        dbworks "users.db":
          ## you can access to `db` var inside this scope
          echo db.getAllRows(sql"...")

    let db {.inject.} = open(path, "", "", "")
    body
    db.close()

template dbworksCapture*(path: string, body): untyped =
    ## similar to `dbworks` but captures the value
    runnableExamples:
        let myRows = dbworksCapture "users.db":
          db.getAllRows(sql"...")
    
    block:
        let
            db {.inject.} = open(path, "", "", "")
            result = body

        db.close()
        result

template transaction*(db, body): untyped =
    db.exec sql"BEGIN"
    body
    db.exec sql"COMMIT"

proc getAllTables*(db: DbConn): seq[string] =
    db.getAllRows("SELECT name FROM sqlite_master WHERE type='table';".sql).mapIt:
        it[0]
