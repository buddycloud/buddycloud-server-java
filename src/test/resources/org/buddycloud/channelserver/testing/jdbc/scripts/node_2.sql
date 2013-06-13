INSERT INTO "nodes" ("node") VALUES ('users/node2@server1/posts');

INSERT INTO "node_config" ("node", "key", "value", "updated") VALUES ('users/node2@server1/posts', 'config1', 'Value of config1', now());
INSERT INTO "node_config" ("node", "key", "value", "updated") VALUES ('users/node2@server1/posts', 'config2', 'Value of config2', now());

INSERT INTO "affiliations" ("node", "user", "affiliation", "updated") VALUES ('users/node2@server1/posts', 'user2@server1', 'owner', now());
INSERT INTO "affiliations" ("node", "user", "affiliation", "updated") VALUES ('users/node2@server1/posts', 'user1@server1', 'publisher', now());

INSERT INTO "subscriptions" ("node", "user", "listener", "subscription", "updated") VALUES ('users/node2@server1/posts', 'user1@server1', 'user1@server1', 'subscribed', now());
INSERT INTO "subscriptions" ("node", "user", "listener", "subscription", "updated") VALUES ('users/node2@server1/posts', 'user1@server2', 'channels.server2', 'subscribed', now());

INSERT INTO "items" ("node", "id", "updated", "xml") VALUES ('users/node2@server1/posts', 'node2:1', TIMESTAMP '2010-01-08 11:45:12', '<entry xmlns="http://www.w3.org/2005/Atom" xmlns:activity="http://activitystrea.ms/spec/1.0/"> 
                <published>2010-01-08T11:45:12Z</published> 
                <author> 
                   <name>user2@server1</name> 
                   <jid xmlns="http://buddycloud.com/atom-elements-0">user2@server1</jid> 
                </author> 
                <content type="text">Test 5</content> 
                <geoloc xmlns="http://jabber.org/protocol/geoloc"> 
                   <text>London, England</text> 
                   <locality>London</locality> 
                   <country>England</country> 
                </geoloc> 
 
                <activity:verb>post</activity:verb>
                <activity:object>
                  <activity:object-type>note</activity:object-type>
                </activity:object>
             </entry>');
