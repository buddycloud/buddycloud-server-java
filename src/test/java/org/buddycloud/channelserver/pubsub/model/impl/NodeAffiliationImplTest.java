package org.buddycloud.channelserver.pubsub.model.impl;

import java.util.Date;

import junit.framework.Assert;
import nl.jqno.equalsverifier.EqualsVerifier;

import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.junit.Test;
import org.xmpp.packet.JID;

public class NodeAffiliationImplTest {

  private String node = "/user/test@example.com/posts";
  private JID fullJid = new JID("test@example.com/resource");

  @Test
  public void testEquals() {
    EqualsVerifier.forClass(NodeAffiliationImpl.class).verify();
  }

  @Test
  public void providingFullJidSetsAsAsBareJid() throws Exception {
    NodeAffiliationImpl affiliation =
        new NodeAffiliationImpl(node, fullJid, Affiliations.member, new Date());
    Assert.assertEquals(fullJid.toBareJID(), affiliation.getUser().toString());
  }

}
