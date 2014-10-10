package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.ArrayList;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.validate.AtomEntry;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class PublishTest extends IQTestHandler {

    private IQ request;
    private ChannelManager channelManager;
    private Publish publish;
    private JID jid;
    private Element element;
    private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
    private String node = "/user/romeo@shakespeare.lit/posts";
    private String server = "channels.shakespeare.lit";
    private AtomEntry validateEntry;
    private Element entry;

    @Before
    public void setUp() throws Exception {
        channelManager = Mockito.mock(ChannelManager.class);
        validateEntry = Mockito.mock(AtomEntry.class);

        Configuration.getInstance().putProperty(Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER, Boolean.TRUE.toString());

        queue = new LinkedBlockingQueue<Packet>();
        publish = new Publish(queue, channelManager);
        jid = new JID("juliet@shakespeare.lit/balcony");
        request = readStanzaAsIq("/iq/pubsub/publish/request.stanza");

        publish.setServerDomain("shakespeare.lit");
        publish.setChannelManager(channelManager);
        publish.setEntryValidator(validateEntry);

        entry = request.getChildElement().element("publish").element("item").element("entry").createCopy();

        element = new BaseElement("publish");

        Mockito.when(channelManager.nodeExists(node)).thenReturn(true);

        NodeMembership membership = new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.publisher, null);
        Mockito.when(channelManager.getNodeMembership(Mockito.eq(node), Mockito.eq(jid))).thenReturn(membership);

        Mockito.when(channelManager.getNodeSubscriptionListeners(Mockito.eq(node))).thenReturn(
                new ResultSetImpl<NodeSubscription>(new ArrayList<NodeSubscription>()));

        validateEntry.setPayload(request.getChildElement().element("publish").element("item").createCopy());
        Mockito.when(validateEntry.getGlobalItemId()).thenReturn(
                new GlobalItemIDImpl(new JID(request.getTo().toBareJID()), node, entry.elementText("id")).toString());

        Mockito.when(validateEntry.getLocalItemId()).thenCallRealMethod();
        Mockito.when(validateEntry.isValid()).thenReturn(true);

        Mockito.when(validateEntry.getPayload()).thenReturn(entry);

    }

    @Test
    public void passingRetractAsElementNameReturnsTrue() {
        Element element = new BaseElement("publish");
        Assert.assertTrue(publish.accept(element));
    }

    @Test
    public void passingNotRetractAsElementNameReturnsFalse() {
        Element element = new BaseElement("not-publish");
        Assert.assertFalse(publish.accept(element));
    }

    @Test
    public void passingNoNodeResultsInErrorStanza() throws Exception {

        IQ request = this.request.createCopy();
        request.getChildElement().element("publish").attribute("node").detach();

        publish.process(element, jid, request, null);

        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.modify, error.getType());
        Assert.assertEquals(XMLConstants.NODE_ID_REQUIRED, error.getApplicationConditionName());

    }

    @Test
    public void nodeStoreExceptionReturnsErrorStanza() throws Exception {
        Mockito.doThrow(new NodeStoreException()).when(channelManager).nodeExists(Mockito.eq(node));

        publish.process(element, jid, request, null);

        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Condition.internal_server_error, error.getCondition());
        Assert.assertEquals(PacketError.Type.wait, error.getType());

    }

    @Test
    public void providingNodeWhichDoesntExistReturnsError() throws Exception {
        Mockito.when(channelManager.nodeExists(node)).thenReturn(false);

        publish.process(element, jid, request, null);

        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.cancel, error.getType());
        Assert.assertEquals(PacketError.Condition.item_not_found, error.getCondition());
    }

    @Test
    public void requestToRemoteNodeResultsInForwardedPacket() throws Exception {
        Configuration.getInstance().remove(Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER);
        Configuration.getInstance().putProperty(Configuration.CONFIGURATION_SERVER_DOMAIN, "shakespeare.lit");

        Assert.assertEquals(new JID("channels.shakespeare.lit"), request.getTo());

        request.getElement().element("pubsub").element("publish").addAttribute("node", "/user/romeo@barracks.lit/posts");
        publish.process(element, jid, request, null);

        Assert.assertEquals(1, queue.size());

        Packet response = queue.poll();

        Assert.assertEquals(new JID("barracks.lit"), response.getTo());
    }

    @Test
    public void unsubscribedUserCanNotPublish() throws Exception {
        NodeMembership membership = new NodeMembershipImpl(node, jid, Subscriptions.none, Affiliations.publisher, null);
        Mockito.when(channelManager.getNodeMembership(Mockito.eq(node), Mockito.eq(jid))).thenReturn(membership);

        publish.process(element, jid, request, null);

        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.auth, error.getType());
        Assert.assertEquals(PacketError.Condition.forbidden, error.getCondition());
    }

    @Test
    public void pendingSubscriptionCanNotPublish() throws Exception {
        NodeMembership membership = new NodeMembershipImpl(node, jid, Subscriptions.pending, Affiliations.publisher, null);
        Mockito.when(channelManager.getNodeMembership(Mockito.eq(node), Mockito.eq(jid))).thenReturn(membership);

        publish.process(element, jid, request, null);

        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.auth, error.getType());
        Assert.assertEquals(PacketError.Condition.forbidden, error.getCondition());
    }

    @Test
    public void noAffiliationCanNotPublish() throws Exception {
        NodeMembership membership = new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.none, null);
        Mockito.when(channelManager.getNodeMembership(Mockito.eq(node), Mockito.eq(jid))).thenReturn(membership);

        publish.process(element, jid, request, null);

        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.auth, error.getType());
        Assert.assertEquals(PacketError.Condition.forbidden, error.getCondition());
    }

    @Test
    public void memberAffiliationCanNotPublish() throws Exception {
        NodeMembership membership = new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.member, null);
        Mockito.when(channelManager.getNodeMembership(Mockito.eq(node), Mockito.eq(jid))).thenReturn(membership);

        publish.process(element, jid, request, null);

        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.auth, error.getType());
        Assert.assertEquals(PacketError.Condition.forbidden, error.getCondition());
    }

    @Test
    public void outcastAffiliationCanNotPublish() throws Exception {
        NodeMembership membership = new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.outcast, null);
        Mockito.when(channelManager.getNodeMembership(Mockito.eq(node), Mockito.eq(jid))).thenReturn(membership);

        publish.process(element, jid, request, null);

        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.auth, error.getType());
        Assert.assertEquals(PacketError.Condition.forbidden, error.getCondition());
    }

    @Test
    public void noItemElementReturnsError() throws Exception {
        IQ request = this.request.createCopy();
        request.getChildElement().element("publish").element("item").detach();

        publish.process(element, jid, request, null);

        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.modify, error.getType());
        Assert.assertEquals(PacketError.Condition.bad_request, error.getCondition());
        Assert.assertEquals(XMLConstants.ITEM_REQUIRED_ELEM, error.getApplicationConditionName());

    }

    @Test
    public void invalidEntryReturnsError() throws Exception {

        String errorMessage = "errorMessage";
        Mockito.when(validateEntry.isValid()).thenReturn(false);
        Mockito.when(validateEntry.getErrorMessage()).thenReturn(errorMessage);
        publish.process(element, jid, request, null);

        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.modify, error.getType());
        Assert.assertEquals(PacketError.Condition.bad_request, error.getCondition());
        Assert.assertEquals(errorMessage, error.getApplicationConditionName());
    }

    @Test
    public void itemIsSavedAsExpected() throws Exception {
        IQ request = this.request.createCopy();
        publish.process(element, jid, request, null);

        Mockito.verify(channelManager, Mockito.times(1)).addNodeItem(Mockito.any(NodeItemImpl.class));
    }

    @Test
    public void expectedSuccessResponseReceived() throws Exception {
        IQ request = this.request.createCopy();
        publish.process(element, jid, request, null);

        IQ response = (IQ) queue.poll();

        Assert.assertEquals(IQ.Type.result, response.getType());
        Assert.assertEquals(request.getFrom(), response.getTo());
        Assert.assertEquals(request.getTo(), response.getFrom());

        Element pubsub = response.getElement().element("pubsub");
        Assert.assertEquals(JabberPubsub.NAMESPACE_URI, pubsub.getNamespaceURI());
        Element publish = pubsub.element("publish");
        Assert.assertEquals(node, publish.attributeValue("node"));

        Element item = publish.element("item");
        Assert.assertNotNull(item);

        Assert.assertTrue(item.attributeValue("id").length() > 0);
        Assert.assertTrue(GlobalItemIDImpl.isGlobalId(item.attributeValue("id")));
    }

    @Test
    public void sendsOutExpectedNotifications() throws Exception {

        NodeSubscription subscriber1 = new NodeSubscriptionImpl(node, new JID("romeo@shakespeare.lit"), Subscriptions.subscribed, null);
        // Expect not to see this user (subscription: 'pending')
        NodeSubscription subscriber2 = new NodeSubscriptionImpl(node, new JID("titania@shakespeare.lit"), Subscriptions.pending, null);
        NodeSubscription subscriber3 =
                new NodeSubscriptionImpl(node, new JID("faustus@marlowe.lit"), new JID("channels.marlowe.lit"), Subscriptions.subscribed, null);

        ArrayList<NodeSubscription> subscribers = new ArrayList<NodeSubscription>();
        subscribers.add(subscriber1);
        subscribers.add(subscriber2);
        subscribers.add(subscriber3);

        Mockito.when(channelManager.getNodeSubscriptionListeners(Mockito.eq(node))).thenReturn(new ResultSetImpl<NodeSubscription>(subscribers));

        IQ request = this.request.createCopy();
        publish.process(element, jid, request, null);

        Assert.assertEquals(5, queue.size());

        queue.poll();
        Message notification = (Message) queue.poll();

        Assert.assertEquals(Message.Type.headline, notification.getType());
        Assert.assertEquals(subscriber1.getUser(), notification.getTo());
        Assert.assertEquals(server, notification.getFrom().toString());

        Element event = notification.getElement().element("event");
        Assert.assertEquals(JabberPubsub.NS_PUBSUB_EVENT, event.getNamespaceURI());

        Element items = event.element("items");
        Assert.assertEquals(node, items.attributeValue("node"));

        Element item = items.element("item");
        Assert.assertTrue(item.attributeValue("id").length() > 0);
        Assert.assertTrue(GlobalItemIDImpl.isGlobalId(item.attributeValue("id")));

        Element responseEntry = item.element("entry");
        Assert.assertEquals(entry.asXML(), responseEntry.asXML());

        notification = (Message) queue.poll();
        Assert.assertEquals(subscriber3.getListener(), notification.getTo());

        notification = (Message) queue.poll();
        Assert.assertEquals(new JID("user1@server1"), notification.getTo());

        notification = (Message) queue.poll();
        Assert.assertEquals(new JID("user2@server1"), notification.getTo());

    }

    @Test
    public void inReplyToIdIsSavedToDatabase() throws Exception {
        IQ request = readStanzaAsIq("/iq/pubsub/publish/reply.stanza");
        Mockito.when(validateEntry.getPayload()).thenReturn(request.getChildElement().element("publish").element("item").element("entry"));

        Mockito.when(validateEntry.getInReplyTo()).thenReturn(
                GlobalItemIDImpl.toLocalId(request.getChildElement().element("publish").element("item").element("entry").element("in-reply-to")
                        .attributeValue("ref")));

        publish.process(element, jid, request, null);

        Assert.assertEquals(IQ.Type.result, ((IQ) queue.poll()).getType());

        ArgumentCaptor<NodeItemImpl> argument = ArgumentCaptor.forClass(NodeItemImpl.class);

        Mockito.verify(channelManager, Mockito.times(1)).addNodeItem(argument.capture());

        Assert.assertEquals("fc362eb42085f017ed9ccd9c4004b095", argument.getValue().getInReplyTo());
        Assert.assertEquals(node, argument.getValue().getNodeId());
    }

    @Test
    public void replyUpdatesThreadParentDate() throws Exception {
        IQ request = readStanzaAsIq("/iq/pubsub/publish/reply.stanza");
        Mockito.when(validateEntry.getPayload()).thenReturn(request.getChildElement().element("publish").element("item").element("entry"));

        Mockito.when(validateEntry.getInReplyTo()).thenReturn("fc362eb42085f017ed9ccd9c4004b095");

        publish.process(element, jid, request, null);

        Assert.assertEquals(IQ.Type.result, ((IQ) queue.poll()).getType());

        ArgumentCaptor<String> inReplyTo = ArgumentCaptor.forClass(String.class);
        ArgumentCaptor<String> passedNode = ArgumentCaptor.forClass(String.class);

        Mockito.verify(channelManager, Mockito.times(1)).updateThreadParent(passedNode.capture(), inReplyTo.capture());

        Assert.assertEquals("fc362eb42085f017ed9ccd9c4004b095", inReplyTo.getValue());
        Assert.assertEquals(node, passedNode.getValue());
    }

    @Test
    public void doesNotUpdateParentThreadIfNotReply() throws Exception {
        IQ request = this.request.createCopy();
        publish.process(element, jid, request, null);

        Mockito.verify(channelManager, Mockito.times(0)).updateThreadParent(Mockito.anyString(), Mockito.anyString());
    }
}
