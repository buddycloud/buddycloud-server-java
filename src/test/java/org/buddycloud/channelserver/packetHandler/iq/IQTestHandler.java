package org.buddycloud.channelserver.packetHandler.iq;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.TestHelper;
import org.dom4j.DocumentException;
import org.xmpp.packet.IQ;

public class IQTestHandler extends TestHandler
{
	public void featureNotImplementedSuccess()
        throws IOException, InterruptedException, DocumentException
    {    
        IQ request = readStanzaAsIq("/iq/featureNotImplemented/request.stanza");
        String expectedReply = readStanzaAsString("/iq/featureNotImplemented/reply.stanza");
        
        TestHelper helper = new TestHelper();
        
        
        helper.getInQueue().put(request);
        
        IQ replyIQ = (IQ) helper.getOutQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        Assert.assertEquals(expectedReply, replyIQ.toXML());
    }
}
