package org.buddycloud.channelserver.pubsub.model.impl;

import java.util.Map;

import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeMembershipWithConfiguration;

public class NodeMembershipWithConfigurationImpl implements NodeMembershipWithConfiguration {

  private final NodeMembership membership;
  private final Map<String, String> configuration;
  
  public NodeMembershipWithConfigurationImpl(NodeMembership membership,
      Map<String, String> configuration) {
    this.membership = membership;
    this.configuration = configuration;
  }

  @Override
  public NodeMembership getMembership() {
    return membership;
  }

  @Override
  public Map<String, String> getConfiguration() {
    return configuration;
  }
  
  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof NodeMembershipWithConfigurationImpl)) {
      return false;
    }
    return this.membership.equals(
        ((NodeMembershipWithConfigurationImpl)obj).getMembership());
  }
  
  public void putConfiguration(String key, String value) {
    configuration.put(key, value);
  }

  @Override
  public String getUID() {
    return this.membership.getUID();
  }

}
