package org.buddycloud.channelserver.channel;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.node.configuration.field.ContentType;
import org.buddycloud.channelserver.channel.validate.AtomEntry;
import org.buddycloud.channelserver.channel.validate.UnknownContentTypeException;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.TestHandler;
import org.buddycloud.channelserver.utils.node.item.payload.Atom;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

public class ValidatePayloadTest extends TestHandler {

	private ValidatePayload validator;
	private ChannelManager channelManager;
	private String node = "/user/doc@outati.me/posts";
	
	@Before
	public void setUp() throws Exception {

		channelManager = Mockito.mock(ChannelManager.class);
		
		Mockito.when(
				channelManager.getNodeConfValue(node, ContentType.FIELD_NAME))
				  .thenReturn(Atom.NS);
		
		validator = new ValidatePayload(channelManager, node);
	}
	
	@Test
	public void whenContentTypeIsAtomNsThenAtomValidatorReturned() throws Exception {
		Assert.assertTrue(validator.getValidator() instanceof AtomEntry);
	}
	
	@Test
	public void nullContentTypeFieldRetunsAtomValidator() throws Exception {
		Mockito.when(
				channelManager.getNodeConfValue(node, ContentType.FIELD_NAME))
				  .thenReturn(null);
		Assert.assertTrue(validator.getValidator() instanceof AtomEntry);
	}
	
	@Test(expected=UnknownContentTypeException.class)
	public void unknownContentTypeThrowsException() throws Exception {
		Mockito.when(
				channelManager.getNodeConfValue(node, ContentType.FIELD_NAME))
				  .thenReturn(Atom.NS_THREAD);
		validator.getValidator();
	}
	
	@Test(expected=NodeStoreException.class)
	public void throwsNodeStoreException() throws Exception {
		Mockito.when(
				channelManager.getNodeConfValue(node, ContentType.FIELD_NAME))
				  .thenThrow(new NodeStoreException());
		validator.getValidator();
	}

}
