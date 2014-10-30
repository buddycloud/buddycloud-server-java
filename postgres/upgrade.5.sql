BEGIN TRANSACTION;

CREATE INDEX "affiliations_user_node" ON affiliations("user","node");
CREATE INDEX "subscriptions_user_node" ON subscriptions("user","node");

INSERT INTO schema_version (version, "when", description)
       VALUES (5, 'now', 'Added indexes for faster queries');

COMMIT;
