
CREATE TABLE "author" (
  "auID" INTEGER PRIMARY KEY AUTOINCREMENT,
  "authorOfInterest" INTEGER DEFAULT 0,
  "modified" TEXT
);

CREATE TABLE "authorName" (
  "anID" INTEGER PRIMARY KEY AUTOINCREMENT,
  "auID" INTEGER NOT NULL,
  "default" INTEGER DEFAULT 0,
  "lastName" TEXT,
  "firstName" TEXT,
  "initials" TEXT,
  "collectiveName" TEXT,
  FOREIGN KEY("auID") REFERENCES "author"("auID") ON DELETE CASCADE
);

CREATE TABLE "article" (
  "arID" INTEGER PRIMARY KEY AUTOINCREMENT,
  "PMID" TEXT UNIQUE,
  "title" TEXT NOT NULL,
  "journal" TEXT,
  "year" INTEGER,
  "month" TEXT,
  "day" INTEGER
);

CREATE TABLE "coAuthor" (
  "arID" INTEGER,
  "auID" INTEGER,
  "authorOrder" INTEGER,
  "anID" INTEGER,
  PRIMARY KEY ("arID", "auID", "authorOrder"),
  FOREIGN KEY("arID") REFERENCES "article"("arID") ON DELETE CASCADE,
  FOREIGN KEY("auID") REFERENCES "author"("auID") ON DELETE CASCADE,
  FOREIGN KEY("anID") REFERENCES "authorName"("anID") ON DELETE CASCADE
);

CREATE TABLE "affiliation" (
  "afID" INTEGER PRIMARY KEY AUTOINCREMENT,
  "affiliation" TEXT NOT NULL
);

CREATE TABLE "author_affiliation" (
  "aafID" INTEGER PRIMARY KEY AUTOINCREMENT,
  "arID" INTEGER NOT NULL,
  "auID" INTEGER NOT NULL,
  "afID" INTEGER NOT NULL,
  FOREIGN KEY("arID") REFERENCES "article"("arID") ON DELETE CASCADE,
  FOREIGN KEY("auID") REFERENCES "author"("auID") ON DELETE CASCADE,
  FOREIGN KEY("afID") REFERENCES "affiliation"("afID") ON DELETE CASCADE
);

CREATE TABLE "meshLink" (
 "uid" INTEGER PRIMARY KEY,
 "meshui" TEXT UNIQUE NOT NULL
);

CREATE TABLE "meshTerm" (
 "mteID" INTEGER PRIMARY KEY AUTOINCREMENT,
 "meshui" TEXT NOT NULL,
 "meshterm" TEXT NOT NULL,
 FOREIGN KEY("meshui") REFERENCES "meshLink"("meshui") ON DELETE CASCADE
);

CREATE TABLE "meshTree" (
 "mtrID" INTEGER PRIMARY KEY AUTOINCREMENT,
 "uid" INTEGER,
 "treenum" TEXT UNIQUE, 
 FOREIGN KEY("uid") REFERENCES "meshLink"("uid") ON DELETE CASCADE
);

CREATE TABLE "mesh_article" (
  "maID" INTEGER PRIMARY KEY AUTOINCREMENT,
  "arID" INTEGER NOT NULL,
  "meshui" TEXT NOT NULL,
  "descriptorMajor" INTEGER,
  FOREIGN KEY("arID") REFERENCES "article"("arID") ON DELETE CASCADE,
  FOREIGN KEY("meshui") REFERENCES "meshLink"("meshui") ON DELETE CASCADE
);

CREATE TABLE "updateData" (
  "uID" INTEGER PRIMARY KEY AUTOINCREMENT,
  "timestamp" TEXT NOT NULL,
  "action" INTEGER
);

CREATE UNIQUE INDEX "idx_treenum" ON "meshTree" ("treenum");

INSERT INTO meshLink ("uid", "meshui") VALUES
  (1,'R000001'),
  (2,'R000002'),
  (3,'R000003'),
  (4,'R000004'),
  (5,'R000005'),
  (6,'R000006'),
  (7,'R000007'),
  (8,'R000008'),
  (9,'R000009'),
  (10,'R000010'),
  (11,'R000011'),
  (12,'R000012'),
  (13,'R000013'),
  (14,'R000014'),
  (15,'R000015'),
  (16,'R000016');

INSERT INTO meshTree ("uid", "treenum") VALUES
  (1, 'A'),
  (2, 'B'),
  (3, 'C'),
  (4, 'D'),
  (5, 'E'),
  (6, 'F'),
  (7, 'G'),
  (8, 'H'),
  (9, 'I'),
  (10, 'J'),
  (11, 'K'),
  (12, 'L'),
  (13, 'M'),
  (14, 'N'),
  (15, 'V'),
  (16, 'Z');

INSERT INTO meshTerm ("meshui", "meshterm") VALUES
  ('R000001', 'Anatomy'),
  ('R000002', 'Organisms'),
  ('R000003', 'Diseases'),
  ('R000004', 'Chemicals and Drugs'),
  ('R000005', 'Analytical, Diagnostic and Therapeutic Techniques, and Equipment'),
  ('R000006', 'Psychiatry and Psychology'),
  ('R000007', 'Phenomena and Processes'),
  ('R000008', 'Disciplines and Occupations'),
  ('R000009', 'Anthropology, Education, Sociology, and Social Phenomena'),
  ('R000010', 'Technology, Industry, and Agriculture'),
  ('R000011', 'Humanities'),
  ('R000012', 'Information Science'),
  ('R000013', 'Named Groups'),
  ('R000014', 'Health Care'),
  ('R000015', 'Publication Characteristics'),
  ('R000016', 'Geographicals');

INSERT INTO updateData ("timestamp", "action") VALUES
  (datetime('now', 'localtime'), 0);
