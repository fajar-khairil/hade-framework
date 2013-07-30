CREATE TABLE "person"( "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, "first_name" TEXT, "last_name" TEXT, "age" INTEGER, "birth" DATE, "company_id" INTEGER );
CREATE TABLE "company" ( "id" INTEGER PRIMARY KEY, "name" TEXT NOT NULL );
BEGIN TRANSACTION;
insert into company ("id", "name") values ('1', 'Hade-Software');
COMMIT;
