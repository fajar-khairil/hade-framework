DROP TABLE IF EXISTS "person";
CREATE TABLE IF NOT EXISTS "person"(
    "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    "first_name" TEXT,
    "last_name" TEXT,
    "age" INTEGER,
    "birth" DATE,
    "company_id" INTEGER	
);
DELETE FROM sqlite_sequence WHERE name = "person";
insert into person ("id", "first_name", "last_name", "age", "birth", "company_id") values ('1', 'Fajar', 'Khairil', '23', '1990-03-02', '1');
insert into person ("id", "first_name", "last_name", "age", "birth", "company_id") values ('2', 'uxmBFcDv I', 'cRKTwUhJeK', '30', '1030-03-02', '1');
insert into person ("id", "first_name", "last_name", "age", "birth", "company_id") values ('3', 'bZgapLPzKx', 'JQvEZfSZjY', '2', '1370-03-02', '1');
insert into person ("id", "first_name", "last_name", "age", "birth", "company_id") values ('4', 'rChVHKOZUs', 'CGVhaIViuj', '24', '1480-03-02', '1');
insert into person ("id", "first_name", "last_name", "age", "birth", "company_id") values ('5', 'vohzgBWLXV', 'saKzIzMlbi', '62', '1060-03-02', '1');
insert into person ("id", "first_name", "last_name", "age", "birth", "company_id") values ('6', 'gwjphnAEzu', 'HszJSEHeRJ', '29', '1370-03-02', '1');
insert into person ("id", "first_name", "last_name", "age", "birth", "company_id") values ('7', 'peMCLCJcQt', 'sbPTMGpIMf', '9', '1890-03-02', '1');
insert into person ("id", "first_name", "last_name", "age", "birth", "company_id") values ('8', 'PVFgxrPxr ', 'zVGkidPlTc', '42', '1460-03-02', '1');
insert into person ("id", "first_name", "last_name", "age", "birth", "company_id") values ('9', 'ATo tjhLko', 'LIdnHmRQAp', '37', '1770-03-02', '1');
insert into person ("id", "first_name", "last_name", "age", "birth", "company_id") values ('10', ' yAZC LIKO', 'iFXXrRvyqd', '62', '1530-03-02', '1');
