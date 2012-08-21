package org.buddycloud.channelserver.pubsub.subscription;

import java.util.Iterator;

public class NodeSubscriptionMock 
    implements NodeSubscription
{
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
		return "romeo@shakespeare.lit";
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