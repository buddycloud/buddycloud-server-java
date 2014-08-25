BEGIN TRANSACTION;

CREATE TABLE "online_users" ("user" TEXT NOT NULL,
			  "updated" TIMESTAMP);

INSERT INTO schema_version (version, "when", description)
       VALUES (7, 'now', 'Added presence table');

COMMIT;
