INSERT INTO "nodes" ("node") VALUES ('/users/subscribed@server1/posts');
INSERT INTO "nodes" ("node") VALUES ('/users/unsubscribed@server1/posts');
INSERT INTO "nodes" ("node") VALUES ('/users/pending@server1/posts');
INSERT INTO "nodes" ("node") VALUES ('/users/subscribed@server1/status');

INSERT INTO "subscriptions" ("node", "user", "listener", "subscription", "updated") 
    VALUES ('/users/subscribed@server1/posts', 'user1@server1', 'user1@server1', 'subscribed', current_timestamp - interval '4' second);
INSERT INTO "subscriptions" ("node", "user", "listener", "subscription", "updated") 
    VALUES ('/users/subscribed@server1/status', 'user2@server1', 'user2@server1', 'subscribed', current_timestamp - interval '3' second);
INSERT INTO "subscriptions" ("node", "user", "listener", "subscription", "updated") 
    VALUES ('/users/pending@server1/posts', 'user2@server1', 'user2@server1', 'pending', current_timestamp - interval '3' second);    

-- The strange order of insertion of the items is deliberate
      
-- author@server1
-- not-author@server1
INSERT INTO "items" ("node", "id", "updated", "xml") 
VALUES ('/users/subscribed@server1/posts', 'a1', TIMESTAMP '2010-01-08 11:45:12', 
               '<entry xmlns="http://www.w3.org/2005/Atom" xmlns:activity="http://activitystrea.ms/spec/1.0/"> 
                <published>2010-01-08T11:45:12Z</published> 
                <author> 
                   <name>author@server1</name> 
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
             
INSERT INTO "items" ("node", "id", "updated", "xml") 
VALUES ('/users/unsubscribed@server1/posts', 'a2', TIMESTAMP '2010-01-08 11:45:12', 
               '<entry xmlns="http://www.w3.org/2005/Atom" xmlns:activity="http://activitystrea.ms/spec/1.0/"> 
                <published>2010-01-08T11:45:12Z</published> 
                <author> 
                   <name>author@server1</name> 
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
       
INSERT INTO "items" ("node", "id", "updated", "xml") 
VALUES ('/users/pending@server1/posts', 'a3', TIMESTAMP '2010-01-08 11:45:12', 
               '<entry xmlns="http://www.w3.org/2005/Atom" xmlns:activity="http://activitystrea.ms/spec/1.0/"> 
                <published>2010-01-08T11:45:12Z</published> 
                <author> 
                   <name>author@server1</name> 
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
             
INSERT INTO "items" ("node", "id", "updated", "xml") 
VALUES ('/users/subscribed@server1/status', 'a4', TIMESTAMP '2010-01-08 11:45:12', 
               '<entry xmlns="http://www.w3.org/2005/Atom" xmlns:activity="http://activitystrea.ms/spec/1.0/"> 
                <published>2010-01-08T11:45:12Z</published> 
                <author> 
                   <name>author@server1</name> 
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