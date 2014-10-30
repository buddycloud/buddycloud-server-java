BEGIN TRANSACTION;

-- Ensure there really are listeners

ALTER TABLE subscriptions ADD COLUMN "invited_by" TEXT;

INSERT INTO schema_version (version, "when", description)
       VALUES (4, 'now', 'Add invited by jid to the database');

COMMIT;
