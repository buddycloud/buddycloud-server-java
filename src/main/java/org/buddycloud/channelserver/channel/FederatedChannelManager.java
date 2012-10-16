package org.buddycloud.channelserver.channel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.TimeoutException;

import org.buddycloud.channelserver.connection.XMPPConnection;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.federation.requests.pubsub.GetUserAffiliationsProcessor;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliation;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.PacketError;

public class FederatedChannelManager implements ChannelManager {

	private final AsyncChannelManager delegate;
	private final XMPPConnection xmppConnection;
	
	public FederatedChannelManager(AsyncChannelManager delgate, final XMPPConnection xmppConnection) {
		this.delegate = delgate;
		this.xmppConnection = xmppConnection;
	}
	
	@Override
	public void createNode(JID owner, String nodeId,
			Map<String, String> nodeConf) throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setNodeConfValue(String nodeId, String key, String value)
			throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setNodeConf(String nodeId, Map<String, String> conf)
			throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public String getNodeConfValue(String nodeId, String key)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Map<String, String> getNodeConf(String nodeId)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean nodeExists(String nodeId) throws NodeStoreException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void setUserAffiliation(String nodeId, JID user,
			Affiliations affiliation) throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void addUserSubscription(NodeSubscription subscription)
			throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public NodeAffiliation getUserAffiliation(String nodeId, JID user)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<NodeAffiliation> getUserAffiliations(JID user)
			throws NodeStoreException {
		final Thread thread = Thread.currentThread();
		
		final ArrayList<Collection<NodeAffiliation>> result = new ArrayList<Collection<NodeAffiliation>>(1);
		final ArrayList<PacketError> error = new ArrayList<PacketError>(1);
		
		delegate.getUserAffiliations(new GetUserAffiliationsProcessor.Handler() {
			
			@Override
			public void onSuccess(Collection<NodeAffiliation> affiliations) {
				result.set(0, affiliations);
				thread.interrupt();
			}

			@Override
			public void onError(PacketError packetError) {
				error.set(0, packetError);
			}
		}, user);
		
		try {
			Thread.sleep(60000);
		} catch(InterruptedException e) {
			if(!result.isEmpty()) {
				return result.get(0);
			}
			
			throw new NodeStoreException(error.get(0).getText());
		}
		
		throw new NodeStoreException();
	}

	@Override
	public Collection<NodeAffiliation> getNodeAffiliations(String nodeId)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<NodeSubscription> getUserSubscriptions(JID user)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<NodeSubscription> getNodeSubscriptions(String nodeId)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NodeSubscription getUserSubscription(String nodeId, JID user)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public CloseableIterator<NodeItem> getNodeItems(String nodeId,
			String afterItemId, int count) throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public CloseableIterator<NodeItem> getNodeItems(String nodeId)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int countNodeItems(String nodeId) throws NodeStoreException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public NodeItem getNodeItem(String nodeId, String nodeItemId)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void addNodeItem(NodeItem item) throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void updateNodeItem(NodeItem item) throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void deleteNodeItemById(String nodeId, String nodeItemId)
			throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void close() throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public Transaction beginTransaction() throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void createPersonalChannel(JID ownerJID) throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean isLocalNode(String nodeId) throws NodeStoreException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isLocalJID(JID jid) throws NodeStoreException {
		// TODO Auto-generated method stub
		return false;
	}

}
