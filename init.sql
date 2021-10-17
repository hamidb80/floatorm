CREATE TABLE members(
    id INTEGER NOT NULL,
    name TEXT(255) NOT NULL,
    PRIMARY KEY (id)
);
CREATE TABLE part(
    id INTEGER NOT NULL,
    name TEXT NOT NULL,
    PRIMARY KEY (id)
);
CREATE TABLE quiz(
    id INTEGER NOT NULL,
    member_id INTEGER NOT NULL,
    name CHAR(255) NOT NULL,
    part_id INTEGER NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (member_id) REFERENCES members (id),
    FOREIGN KEY (part_id) REFERENCES part (id)
);
CREATE TABLE question(
    id INTEGER NOT NULL,
    quiz_id INTEGER NOT NULL,
    answer INTEGER NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (quiz_id) REFERENCES quiz (id)
);
CREATE TABLE record(
    id INTEGER NOT NULL,
    answerts CHAR(400) DEFAULT 100,
    question_order CHAR(400) NOT NULL,
    member_id INTEGER NOT NULL,
    date CHAR(400) NOT NULL DEFAULT "val",
    PRIMARY KEY (id, question_order, date),
    FOREIGN KEY (member_id) REFERENCES members (id)
);
CREATE TABLE test(
    field CHAR(255)
);