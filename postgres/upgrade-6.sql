BEGIN TRANSACTION;

ALTER TABLE items ADD COLUMN "created" TIMESTAMP;

INSERT INTO schema_version (version, "when", description)
       VALUES (6, 'now', 'Add created date on items');

COMMIT;
