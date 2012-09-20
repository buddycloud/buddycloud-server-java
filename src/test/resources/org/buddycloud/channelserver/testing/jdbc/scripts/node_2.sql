INSERT INTO "nodes" ("node") VALUES ('users/node2@server1/posts');

INSERT INTO "node_config" ("node", "key", "value", "updated") VALUES ('users/node2@server1/posts', 'config1', 'Value of config1', now());
INSERT INTO "node_config" ("node", "key", "value", "updated") VALUES ('users/node2@server1/posts', 'config2', 'Value of config2', now());

INSERT INTO "affiliations" ("node", "user", "affiliation", "updated") VALUES ('users/node2@server1/posts', 'user2@server1', 'owner', now());

INSERT INTO "subscriptions" ("node", "user", "listener", "subscription", "updated") VALUES ('users/node2@server1/posts', 'user1@server1', 'user1@server1', 'subscribed', now());
INSERT INTO "subscriptions" ("node", "user", "listener", "subscription", "updated") VALUES ('users/node2@server1/posts', 'user1@server2', 'channels.server2', 'subscribed', now());
