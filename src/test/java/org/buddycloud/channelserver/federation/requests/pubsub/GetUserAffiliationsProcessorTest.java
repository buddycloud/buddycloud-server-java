package org.buddycloud.channelserver.federation.requests.pubsub;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class GetUserAffiliationsProcessorTest {

	private GetUserAffiliationsProcessor processor;
	private JID jid;

	@Before
	public void setUp() throws Exception {
		jid = new JID("romeo@shakespeare.lit");
		processor = new GetUserAffiliationsProcessor(jid);
	}

	@Test
	public void testCanGenerateRequiredRequestStanza() {
		IQ request = processor.getRequest();
System.out.println(request.toXML());
		Assert.assertEquals(jid.toBareJID(), request.getChildElement().element("actor")
				.attributeValue("jid"));
	}
}
