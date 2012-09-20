package org.buddycloud.channelserver.pubsub.subscription;

import org.buddycloud.channelserver.pubsub.model.NodeSubscription;

public class NodeSubscriptionMock 
    implements NodeSubscription
{
	private String subscriber;
	
	public NodeSubscriptionMock(String jid)
	{
		subscriber = jid;
	}

	@Override
	public String getAffiliation()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getSubscription()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getBareJID()
	{
		return subscriber;
	}
	
	@Override
	public String getNode()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getForeignChannelServer()
	{
		// TODO Auto-generated method stub
		return null;
	}
}