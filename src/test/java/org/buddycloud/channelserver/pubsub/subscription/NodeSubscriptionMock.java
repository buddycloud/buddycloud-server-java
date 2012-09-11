package org.buddycloud.channelserver.pubsub.subscription;

public class NodeSubscriptionMock implements NodeSubscription {
	private String subscriber;
	private String affiliation;
	private String subscription;
	private String foreignChannelServer;

	public void setBareJID(String jid) {
		this.subscriber = jid;
	}

	public void setAffiliation(String affiliation) {
		this.affiliation = affiliation;
	}

	public void setSubscription(String subscription) {
		this.subscription = subscription;
	}

	public void setForeignChannelServer(String foreignChannelServer) {
		this.foreignChannelServer = foreignChannelServer;
	}

	public NodeSubscriptionMock(String jid) {
		subscriber = jid;
	}

	@Override
	public String getAffiliation() {
		// TODO Auto-generated method stub
		return affiliation;
	}

	@Override
	public String getSubscription() {
		// TODO Auto-generated method stub
		return subscription;
	}

	@Override
	public String getBareJID() {
		return subscriber;
	}

	@Override
	public String getNode() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getForeignChannelServer() {
		return foreignChannelServer;
	}
}