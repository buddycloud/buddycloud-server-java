package org.buddycloud.channelserver.pubsub.model;

import java.util.Map;

import org.xmpp.resultsetmanagement.Result;

public interface NodeMembershipWithConfiguration extends Result {

  NodeMembership getMembership();
  
  Map<String, String> getConfiguration();
  
}
