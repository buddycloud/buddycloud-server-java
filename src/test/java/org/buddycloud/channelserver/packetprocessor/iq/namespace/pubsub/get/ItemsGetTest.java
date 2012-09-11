package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.AffiliationEvent;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;

public class ItemsGetTest extends IQTestHandler {

	private IQ request;
	private ItemsGet itemsGet;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");
	private Mock dataStore = new Mock();

	@Before
	public void setUp() throws Exception {

		queue    = new LinkedBlockingQueue<Packet>();
		itemsGet = new ItemsGet(queue, dataStore);
		request  = readStanzaAsIq("/iq/pubsub/affiliation/affiliationChange.stanza");
	}
	
	@Test
	public void testPassingAffiliationsAsElementNameReturnsTrue() {
		Element element = new BaseElement("items");
		assertTrue(itemsGet.accept(element));
	}

	@Test
	public void testPassingNotCreateAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-items");
		assertFalse(itemsGet.accept(element));
	}
}