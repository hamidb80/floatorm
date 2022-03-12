template how_to(idea, blueprint) = discard


type
  Email* = distinct string
  FileStates* = enum
    fsUnspecified, fsInProgress, fsFailed, fsFinished

  Percent* = range[0.0 .. 100.0]

how_to "model database":

  func toEmail(s: string): Email {.deserializer.}

  Blueprint():
    Table user {.insert.}:
      id: string {.primary, insert: false.}
      name: string
      email: string {.type: Email.}

    Table file {.insert.}:
      id: int {.primary.}
      uid: int[ref user.id]

      path: string {.type: Path.}
      state: int {.type: FileStates.}
      progress: int {.type: Percent.}


    db.insertUser(name = "ali", email = "hey@support.com")

how_to "define query models":
  ## single def
  model CompleteFileInfo *=
      (FileModel{id, path, state, progress}, user: UserModel{id, name})

  ## multi def
  models:
    CompleteFileInfo *=
      (FileModel{id, path, state, progress}, user: UserModel{id, name})

how_to "query":

  proc getUserFiles(uid: int, state: FileStates): auto =
    sql"""
      SELECT id, path, progress, COUNT(1) as d
      FROM file
      WHERE uid = ? AND state = ?
    """
    .toModel FileModel{id, path, progress, count: int}[]


  proc getFiles: seq[CompleteFileInfo] = sql"""
      SELECT f.id, f.path, f.state, f.progress, u.id, u.name
      FROM file f, user u
      LEFT JOIN user uid = u.id
    """
    .toModel (FileModel{id, path, state, progress}, user: UserModel{id, name})[]
    .toModel (file: FileModel{id, path, }, user: UserModel{id, name})[]
    .toModel (file: {id: FieldMode.id, }, user: {id: int, name: string})[]
    .toModel FileModel{id, path, state, progress, user: UserModel{id, name}}[]


  proc getFileState(id: int): auto = sql"""
      SELECT state
      FROM file
      WHERE id = ?
    """
    .toValue Option[FileModel.state]


  proc getFileState(id: int): auto = sql"""
      SELECT state, progress
      FROM file
      WHERE id = ?
    """
    .toValues FileModel{state, progress}
    .toValues (st: FileModel.state, p: FileModel.progress)
