INSERT INTO "items" ("node", "id", "updated", "xml", "in_reply_to") VALUES ('users/node1@server1/posts', 'a6', TIMESTAMP '2010-01-08 11:45:12', '<entry xmlns="http://www.w3.org/2005/Atom" xmlns:activity="http://activitystrea.ms/spec/1.0/"> 
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
             </entry>', 'a1');

INSERT INTO "items" ("node", "id", "updated", "xml", "in_reply_to") VALUES ('users/node1@server1/posts', 'a7', TIMESTAMP '2010-01-06 22:32:12', '<entry xmlns="http://www.w3.org/2005/Atom" xmlns:activity="http://activitystrea.ms/spec/1.0/"> 
                <published>2010-01-06T22:32:12Z</published> 
                <author> 
                   <name>user1@server1</name> 
                   <jid xmlns="http://buddycloud.com/atom-elements-0">user1@server1</jid> 
                </author> 
                <content type="text">Test 2</content> 
                <geoloc xmlns="http://jabber.org/protocol/geoloc"> 
                   <text>Paris, France</text> 
                   <locality>Paris</locality> 
                   <country>France</country> 
                </geoloc> 
 
                <activity:verb>post</activity:verb>
                <activity:object>
                  <activity:object-type>note</activity:object-type>
                </activity:object>
             </entry>', 'a1');

INSERT INTO "items" ("node", "id", "updated", "xml", "in_reply_to") VALUES ('users/node1@server1/posts', 'a8', TIMESTAMP '2010-01-06 22:32:12', '<entry xmlns="http://www.w3.org/2005/Atom" xmlns:activity="http://activitystrea.ms/spec/1.0/"> 
                <published>2010-01-06T22:32:12Z</published> 
                <author> 
                   <name>user1@server1</name> 
                   <jid xmlns="http://buddycloud.com/atom-elements-0">user1@server1</jid> 
                </author> 
                <content type="text">Test 2</content> 
                <geoloc xmlns="http://jabber.org/protocol/geoloc"> 
                   <text>Paris, France</text> 
                   <locality>Paris</locality> 
                   <country>France</country> 
                </geoloc> 
 
                <activity:verb>post</activity:verb>
                <activity:object>
                  <activity:object-type>note</activity:object-type>
                </activity:object>
             </entry>', NULL);
