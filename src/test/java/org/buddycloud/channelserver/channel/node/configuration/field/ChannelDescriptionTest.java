package org.buddycloud.channelserver.channel.node.configuration.field;

import junit.framework.Assert;

import org.apache.commons.lang.StringUtils;
import org.buddycloud.channelserver.channel.node.configuration.field.ChannelTitle;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.junit.Before;
import org.junit.Test;

public class ChannelDescriptionTest extends IQTestHandler {
	
	private ChannelDescription field;

	@Before
	public void setUp() {
		field = new ChannelDescription();

	}

	@Test
	public void longestTitleIs128Characters()
			throws NodeStoreException {
		
		String testTitle = StringUtils.repeat("Whoa. This is heavy.", ChannelTitle.MAX_TITLE_LENGTH);
		field.setValue(testTitle);
		Assert.assertEquals(field.MAX_DESCRIPTION_LENGTH, field.getValue().length());
	}
	
	@Test
	public void doesNotTrucateShorterStrings() {
		String testTitle = "The way I see it, if you're gonna build a " +
	        "time machine into a car, why not do it with some *style?*";
		field.setValue(testTitle);
		Assert.assertEquals(testTitle, field.getValue());
	}
}