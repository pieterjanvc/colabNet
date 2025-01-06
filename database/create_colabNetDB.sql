
CREATE TABLE IF NOT EXISTS "author" (
  "auID" INTEGER PRIMARY KEY AUTOINCREMENT,
  "modified" TEXT
);

CREATE TABLE IF NOT EXISTS "authorName" (
  "anID" INTEGER PRIMARY KEY AUTOINCREMENT,
  "auID" INTEGER NOT NULL,
  "lastName" TEXT,
  "firstName" TEXT,
  "initials" TEXT,
  "collectiveName" TEXT,
  FOREIGN KEY("auID") REFERENCES "author"("auID")
);

CREATE TABLE IF NOT EXISTS "article" (
  "arID" INTEGER PRIMARY KEY AUTOINCREMENT,
  "PMID" TEXT UNIQUE,
  "title" TEXT NOT NULL,
  "journal" TEXT,
  "year" INTEGER,
  "month" TEXT,
  "day" INTEGER
);

CREATE TABLE IF NOT EXISTS "coAuthor" (
  "arID" INTEGER,
  "auID" INTEGER,
  PRIMARY KEY ("arID", "auID"),
  FOREIGN KEY("arID") REFERENCES "article"("arID"),
  FOREIGN KEY("auID") REFERENCES "author"("auID")
);

CREATE TABLE IF NOT EXISTS "affiliation" (
  "afID" INTEGER PRIMARY KEY AUTOINCREMENT,
  "affiliation" TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS "author_affiliation" (
  "arID" INTEGER NOT NULL,
  "auID" INTEGER NOT NULL,
  "afID" INTEGER NOT NULL,
  FOREIGN KEY("arID") REFERENCES "article"("arID"),
  FOREIGN KEY("auID") REFERENCES "author"("auID"),
  FOREIGN KEY("afID") REFERENCES "affiliation"("afID")
);

CREATE TABLE IF NOT EXISTS "meshLinks" (
 "uid" INTEGER PRIMARY KEY,
 "meshui" TEXT NOT NULL
);


CREATE TABLE IF NOT EXISTS "meshTerms" (
 "mteID" INTEGER PRIMARY KEY AUTOINCREMENT,
 "meshui" INTEGER NOT NULL,
 "meshterm" TEXT NOT NULL,
 FOREIGN KEY("meshui") REFERENCES "meshLinks"("meshui")
);

CREATE TABLE IF NOT EXISTS "meshTree" (
 "mtrID" INTEGER PRIMARY KEY AUTOINCREMENT,
 "uid" INTEGER,
 "treenum" TEXT UNIQUE, 
 FOREIGN KEY("uid") REFERENCES "meshLinks"("uid")
);

CREATE TABLE IF NOT EXISTS "mesh_article" (
  "arID" INTEGER NOT NULL,
  "uid" TEXT NOT NULL,
  FOREIGN KEY("arID") REFERENCES "article"("arID"),
  FOREIGN KEY("uid") REFERENCES "meshLinks"("uid")
);
