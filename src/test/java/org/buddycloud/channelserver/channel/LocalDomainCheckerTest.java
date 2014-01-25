package org.buddycloud.channelserver.channel;

import java.util.Properties;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
import org.junit.Test;

public class LocalDomainCheckerTest {

	@Test
	public void testNullConfiguration() {
		Assert.assertFalse(LocalDomainChecker.isLocal("whatever", 
				null));
	}
	
	@Test
	public void testNullProperty() {
		Assert.assertFalse(LocalDomainChecker.isLocal("whatever", 
				new Properties()));
	}
	
	@Test
	public void testTrueFlag() {
		Properties conf = new Properties();
		conf.setProperty(Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER, "true");
		Assert.assertTrue(LocalDomainChecker.isLocal("whatever", conf));
	}
}
