# Task list:

## Immediate fixes

* Events
   * Need to store remote affiliations / subscriptions in database
   * Update remote subscriptions / affiliations on event receiving
   * Push event notifications to appropriate users
* Handle s2s failure cases (FederatedQueueManager)
* Fix case where remote server dials in before local server has discovered it

## More general fixes

* Write a quick how to and explanation of the "logic"

* Check that user cannot unsubscribe from a node when she is a owner
* Add presence subscription
* Handle error case 6.2.3.2 No Such Subscriber
* Add command :version to return the current version of the channel server.
* when user subscribes to local node, increase subscribers counter.
* when user unsubscribes from local node, decrease subscribers counter.

* Check that this server sends the same as the node.js one (the entry id is not valid though):
  <message type="headline" to="comptest.xmpp.lobstermonster.org" from="buddycloud.highfellow.org">
     <event xmlns="http://jabber.org/protocol/pubsub#event">
        <items node="/user/test@highfellow.org/posts">
           <item id="988ac6f9-7ad6-42ee-9e40-1773eb61e8aa">
              <entry xmlns="http://www.w3.org/2005/Atom">
                 <content>PONG!!!</content>
                 <author>
                    <name>tuomas@buddycloud.org</name>
                    <uri>acct:tuomas@buddycloud.org</uri>
                 </author>
                 <in-reply-to xmlns="http://purl.org/syndication/thread/1.0" ref="cef69083-ecb8-4b1f-bcca-5a774b78c7a5"/>
                 <id>988ac6f9-7ad6-42ee-9e40-1773eb61e8aa</id>
                 <published>2011-11-29T21:16:23.720Z</published>
                 <updated>2011-11-29T21:16:23.720Z</updated>
                 <link rel="self" href="xmpp:buddycloud.highfellow.org?pubsub;action=retrieve;node=/user/test@highfellow.org/posts;item=988ac6f9-7ad6-42ee-9e40-1773eb61e8aa"/>
                 <verb xmlns="http://activitystrea.ms/spec/1.0/">post</verb>
                 <object xmlns="http://activitystrea.ms/spec/1.0/">
                    <object-type>comment</object-type>
                 </object>
              </entry>
           </item>
        </items>
     </event>
  </message>
  
  
  
