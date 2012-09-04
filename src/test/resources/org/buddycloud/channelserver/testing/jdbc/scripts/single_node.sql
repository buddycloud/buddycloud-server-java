INSERT INTO nodes (node) VALUES ("node1");

INSERT INTO node_config (node, key, value, updated) VALUES ("node1", "config1", "Value of config1", now());
INSERT INTO node_config (node, key, value, updated) VALUES ("node1", "config2", "Value of config2", now());

INSERT INTO affiliations (node, user, affiliation, updated) VALUES ("node1", "user1@sample.com", "owner", now());