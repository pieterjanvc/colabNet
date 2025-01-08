
CREATE TABLE "author" (
  "auID" INTEGER PRIMARY KEY AUTOINCREMENT,
  "modified" TEXT
);

CREATE TABLE "authorName" (
  "anID" INTEGER PRIMARY KEY AUTOINCREMENT,
  "auID" INTEGER NOT NULL,
  "lastName" TEXT,
  "firstName" TEXT,
  "initials" TEXT,
  "collectiveName" TEXT,
  FOREIGN KEY("auID") REFERENCES "author"("auID")
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
  PRIMARY KEY ("arID", "auID"),
  FOREIGN KEY("arID") REFERENCES "article"("arID"),
  FOREIGN KEY("auID") REFERENCES "author"("auID")
);

CREATE TABLE "affiliation" (
  "afID" INTEGER PRIMARY KEY AUTOINCREMENT,
  "affiliation" TEXT NOT NULL
);

CREATE TABLE "author_affiliation" (
  "arID" INTEGER NOT NULL,
  "auID" INTEGER NOT NULL,
  "afID" INTEGER NOT NULL,
  FOREIGN KEY("arID") REFERENCES "article"("arID"),
  FOREIGN KEY("auID") REFERENCES "author"("auID"),
  FOREIGN KEY("afID") REFERENCES "affiliation"("afID")
);

CREATE TABLE "meshLink" (
 "uid" INTEGER PRIMARY KEY,
 "meshui" TEXT NOT NULL
);

CREATE TABLE "meshTerm" (
 "mteID" INTEGER PRIMARY KEY AUTOINCREMENT,
 "meshui" INTEGER NOT NULL,
 "meshterm" TEXT NOT NULL,
 FOREIGN KEY("meshui") REFERENCES "meshLink"("meshui")
);

CREATE TABLE "meshTree" (
 "mtrID" INTEGER PRIMARY KEY AUTOINCREMENT,
 "uid" INTEGER,
 "treenum" TEXT UNIQUE, 
 FOREIGN KEY("uid") REFERENCES "meshLink"("uid")
);

CREATE TABLE "mesh_article" (
  "arID" INTEGER NOT NULL,
  "meshui" TEXT NOT NULL,
  "descriptorMajor" INTEGER,
  FOREIGN KEY("arID") REFERENCES "article"("arID"),
  FOREIGN KEY("meshui") REFERENCES "meshLink"("meshui")
);
