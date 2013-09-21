BEGIN TRANSACTION;

-- Ensure there really are listeners

ALTER TABLE subscriptions ALTER COLUMN node set NOT NULL;
ALTER TABLE subscriptions ALTER COLUMN "user" set NOT NULL;
ALTER TABLE subscriptions ALTER COLUMN listener set NOT NULL;
ALTER TABLE subscriptions ALTER COLUMN subscription set NOT NULL;
ALTER TABLE subscriptions ALTER COLUMN updated set NOT NULL;
ALTER TABLE subscriptions ALTER COLUMN anonymous set NOT NULL;
ALTER TABLE subscriptions ALTER COLUMN temporary set NOT NULL;

INSERT INTO schema_version (version, "when", description)
       VALUES (3, 'now', 'Added listeners constraint to subscriptions table');

COMMIT;
