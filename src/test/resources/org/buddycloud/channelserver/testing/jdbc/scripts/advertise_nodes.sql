INSERT INTO "nodes" ("node") VALUES ('/user/advertised@server2/posts');
INSERT INTO "nodes" ("node") VALUES ('/user/not-advertised@server2/posts');
INSERT INTO "nodes" ("node") VALUES ('/user/undetermined-advertised@server2/posts');

INSERT INTO "nodes" ("node") VALUES ('/user/advertised@server1/posts');
INSERT INTO "nodes" ("node") VALUES ('/user/not-advertised@server1/posts');
INSERT INTO "nodes" ("node") VALUES ('/user/undetermined-advertised@server1/posts');

INSERT INTO "node_config" ("node", "key", "value", "updated") VALUES ('/user/advertised@server1/posts', 'buddycloud#advertise_node', 'true', now());

INSERT INTO "node_config" ("node", "key", "value", "updated") VALUES ('/user/not-advertised@server1/posts', 'buddycloud#advertise_node', 'false', now());

INSERT INTO "node_config" ("node", "key", "value", "updated") VALUES ('/user/advertised@server2/posts', 'buddycloud#advertise_node', 'true', now());

INSERT INTO "node_config" ("node", "key", "value", "updated") VALUES ('/user/not-advertised@server2/posts', 'buddycloud#advertise_node', 'false', now());