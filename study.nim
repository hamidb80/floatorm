template `>>`(context, impl) = discard

# --------------------------------------------

>> "restrict model":
  type
    Email = distinct string
    FileStates = enum
      fsPending, fsInProgress, fsFailed, fsFinished

    Percent = range[0.0 .. 100.0]

  func toEmail(s: string): Email


>> "model database":
  let maxUsernameLen = parseInt getEnv "MAX_STR_LEN"

  Blueprint():
    convertors:
      string ~> Email: toEmail

    Table user:
      id: char(maxUsernameLen) {.primary.}
      email: string ~> Email
      meta: ?string ~> JsonNode

    Table file:
      id: int {.primary, name: "_id".}
      uid: int[ref user.id]
      state: int ~> FileStates = fsPending.int
      progress: float ~> Percent = 0.0
      metadata: ?string

    # --------------------------------

    type
      UserModel = object
        id: string
        email: string
        meta: string

    Table{name = "user",
      columns = [
        Column{
          name: "id", `type`: "INT",
          param: Option[string], defaultValue: Option[string],
          features: {cfPrimary, cfNotNull}
        },
      ],
      indexes = [
        Index(fields: @["a", "b"], name: "index_ab")
      ],
      references = [
        Reference(`from`: "a", to: DBPath{table: "_table", column: "_col"})
      ],
    }


>> "define custom models":
  type CompleteFileInfo {.model.} =
    (FileModel{id, path, state, progress}, user: UserModel{id, name})


>> "query":
  proc getFiles: seq[CompleteFileInfo] = sql"""
      SELECT f.id, f.path, f.state, f.progress, u.id, u.name
      FROM file f, user u
      LEFT JOIN user uid = u.id
    """
    .toModel (FileModel{id, path, state, progress}, user: UserModel{id, name})[]
    .toModel (file: FileModel{id, path}, user: UserModel{id, name})[]
    .toModel (file: {id: FieldMode.id}, user: {id: int, name: string})[]
    .toModel FileModel{id, path, state, progress, user: UserModel{id, name}}[]


  proc getFileState(id: int): auto = sql"""
      SELECT state
      FROM file
      WHERE id = ?
    """
    .toValue ?FileModel.state


  proc getFileStatus(id: int): auto = sql"""
      SELECT state, progress
      FROM file
      WHERE id = ?
    """
    .toValues FileModel{state, progress}
    .toValues (s: FileModel.state, p: FileModel.progress)
