BEGIN TRANSACTION;

-- Ensure there really are listeners

ALTER TABLE subscriptions ALTER COLUMN listener set NOT NULL;

INSERT INTO schema_version (version, "when", description)
       VALUES (3, 'now', 'Added listeners constraint to subscriptions table');

COMMIT;
