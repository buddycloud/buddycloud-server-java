package org.buddycloud.channelserver.pubsub.subscription;

import java.util.Date;

import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.xmpp.packet.JID;

public class NodeSubscriptionMock implements NodeSubscription {
	private JID subscriber;
	private Affiliations affiliation;
	private Subscriptions subscription;
	private String foreignChannelServer;

	public void setBareJID(JID jid) {
		this.subscriber = jid;
	}

	public void setAffiliation(Affiliations affiliation) {
		this.affiliation = affiliation;
	}

	public void setSubscription(Subscriptions subscription) {
		this.subscription = subscription;
	}

	public void setForeignChannelServer(String foreignChannelServer) {
		this.foreignChannelServer = foreignChannelServer;
	}

	public NodeSubscriptionMock(JID jid) {
		subscriber = jid;
	}

	@Override
	public Subscriptions getSubscription() {
		// TODO Auto-generated method stub
		return subscription;
	}

	@Override
	public JID getUser() {
		// TODO Auto-generated method stub
		return subscriber;
	}

	@Override
	public JID getListener() {
		// TODO Auto-generated method stub
		return subscriber;
	}

	@Override
	public String getNodeId() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getUID() {
		return subscriber.toString();
	}

	@Override
	public Date getLastUpdated() {
		return new Date();
	}
}