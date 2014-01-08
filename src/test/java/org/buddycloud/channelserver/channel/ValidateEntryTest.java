package org.buddycloud.channelserver.channel;

import org.junit.Test;
import org.buddycloud.channelserver.packetHandler.iq.TestHandler;
import junit.framework.Assert;
import junit.framework.TestCase;

public class ValidateEntryTest extends TestHandler {
	
	ValidateEntry validateEntry;

	@Test
	public void notProvidingAnEntryReturnsError() throws Exception {
		
		validateEntry = new ValidateEntry(null);
		Assert.assertFalse(validateEntry.isValid());
	}
}
