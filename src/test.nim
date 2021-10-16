import easydb

Blueprint []:
    Table test1:
        id: int {primary} 
        table_id: int[ref another.id]
        name: char[255]

    Table members:
        id: int {primary}
        name: string[255]

    Table part:
        id: int {primary}
        name: string

    Table quiz:
        id: int {primary}
        member_id: int[ref members.id]
        name: char[255]
        part_id: int[ref part.id]

    Table question:
        id: int {primary}
        quiz_id: int[ref quiz.id]
        answer: int

    Table record:
        id: int {primary}
        # TODO
        # member_id: int[ref members.id] {update: restrict, delete: restric}
        date: string

    Table test:
        field: Option[string]
