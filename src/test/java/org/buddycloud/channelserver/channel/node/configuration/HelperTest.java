package org.buddycloud.channelserver.channel.node.configuration;

import java.util.HashMap;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.field.ChannelTitle;
import org.buddycloud.channelserver.channel.node.configuration.field.ChannelType;
import org.buddycloud.channelserver.channel.node.configuration.field.ConfigurationFieldException;
import org.buddycloud.channelserver.channel.node.configuration.field.Field;
import org.buddycloud.channelserver.channel.node.configuration.field.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.dom4j.Element;
import org.dom4j.Node;
import org.dom4j.dom.DOMElement;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.buddycloud.channelserver.channel.node.configuration.field.Factory;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;

public class HelperTest extends IQTestHandler
{
	private Helper parser;
	private String node = "/user/user@example.com/posts";
	private ChannelManager channelManager;
	
	@Rule public ExpectedException thrown = ExpectedException.none();
	
	@Before
	public void setUp()
	{
		channelManager = Mockito.mock(ChannelManager.class);
		parser = new Helper(channelManager);
		parser.setNode(node);
	}
    
    @Test(expected=NodeConfigurationException.class)
    public void testPassingPacketWhichDoesntContainConfigureElementThrowsException() 
	{
		Element iq     = new DOMElement("iq");
		iq.addElement("pubsub", JabberPubsub.NS_PUBSUB_OWNER);
		IQ request = new IQ(iq);

	    parser.parse(request);
	}
    
    @Test
    public void testNotProvidingAnyConfigurationFieldsReturnsNoConfiguration() throws NodeStoreException
    {
    	Element iq        = new DOMElement("iq");
    	Element pubsub    = iq.addElement("pubsub");
    	pubsub.addAttribute("xmlns", JabberPubsub.NS_PUBSUB_OWNER);
    	Element configure = pubsub.addElement("configure");
    	configure.addElement("x");
    	IQ request        = new IQ(iq);
    	
    	parser.parse(request);
    	Assert.assertEquals(0, parser.getValues().size());
    }

    @Test(expected=NodeConfigurationException.class)
    public void testBadFieldConfigurationValueThrowsException()
    {
    	Factory factoryMock = Mockito.mock(Factory.class);
    	Mockito.doThrow(new ConfigurationFieldException())
	        .when(factoryMock).create(Mockito.anyString(), Mockito.anyString());
    	parser.setFieldFactory(factoryMock);

    	Element iq          = new DOMElement("iq");
    	Element pubsub      = iq.addElement("pubsub");
    	pubsub.addAttribute("xmlns", JabberPubsub.NS_PUBSUB_OWNER);
    	Element configure   = pubsub.addElement("configure");
    	Element x           = configure.addElement("x");
    	Element field       = x.addElement("field");
    	field.addAttribute("var", ChannelTitle.FIELD_NAME);
    	IQ request          = new IQ(iq);

        parser.parse(request); 
    }
    
    @Test
    public void testPassingSimpleConfigurationReturnsExceptedResults() throws NodeStoreException
    {
    	Mock fieldMock = new Mock();
    	fieldMock.setValue(Mock.DEFAULT_VALUE);
    	Mock fieldMock2 = new Mock("MOCK_FIELD_TWO");
    	fieldMock2.setValue("My field value");
    	Factory factoryMock = Mockito.mock(Factory.class);
		Mockito
	        .when(factoryMock.create(Mockito.anyString(), Mockito.anyString()))
	    	.thenReturn(fieldMock, fieldMock2);
    	parser.setFieldFactory(factoryMock);

    	Element iq          = new DOMElement("iq");
    	Element pubsub      = iq.addElement("pubsub");
    	pubsub.addAttribute("xmlns", JabberPubsub.NS_PUBSUB_OWNER);
    	Element configure   = pubsub.addElement("configure");
    	Element x           = configure.addElement("x");
    	Element field       = x.addElement("field");
    	field.addAttribute("var", Mock.FIELD_NAME);
    	// Won't fail on missing 'var' attribute as that's the factory's job!
    	Element field2      = x.addElement("field");
    	Element value       = field.addElement("value");
    	Element value2      = field2.addElement("value");
    	value.addText(Mock.DEFAULT_VALUE);
    	value2.addText("value2");
    	IQ request          = new IQ(iq);
    	
    	parser.parse(request);
        
    	Assert.assertEquals(2, parser.getValues().size());
    	Assert.assertEquals(
    	    Mock.DEFAULT_VALUE,
    	    parser.getValues().get(fieldMock.getName())
    	);
    	Assert.assertEquals(
    		"My field value",
    		parser.getValues().get(fieldMock2.getName())
    	);
    }
    
    @Test
    public void testAllValidElementsMeansPositiveIsValidCall() throws NodeStoreException
    {
    	Mock fieldMock = Mockito.mock(Mock.class);
    	Mockito
    	    .doReturn(true)
    	    .when(fieldMock)
    	    .isValid();
    	Factory factoryMock = Mockito.mock(Factory.class);
		Mockito
	        .when(factoryMock.create(Mockito.anyString(), Mockito.anyString()))
	    	.thenReturn(fieldMock);
    	parser.setFieldFactory(factoryMock);

    	Element iq          = new DOMElement("iq");
    	Element pubsub      = iq.addElement("pubsub");
    	pubsub.addAttribute("xmlns", JabberPubsub.NS_PUBSUB_OWNER);
    	Element configure   = pubsub.addElement("configure");
    	Element x           = configure.addElement("x");
    	Element field       = x.addElement("field");
    	field.addAttribute("var", Mock.FIELD_NAME);
    	Element value       = field.addElement("value");
    	value.addText(Mock.DEFAULT_VALUE);
    	IQ request          = new IQ(iq);
    	
    	parser.parse(request);
        
    	Assert.assertEquals(1, parser.getValues().size());
    	Assert.assertTrue(parser.isValid());
    }
    
    @Test
    public void testInvalidElementMeansNegativeIsValidCall() throws NodeStoreException
    {
    	Mock fieldMock = Mockito.mock(Mock.class);
    	Mockito
    	    .doReturn(false)
    	    .when(fieldMock)
    	    .isValid();
    	Factory factoryMock = Mockito.mock(Factory.class);
		Mockito
	        .when(factoryMock.create(Mockito.anyString(), Mockito.anyString()))
	    	.thenReturn(fieldMock);
    	parser.setFieldFactory(factoryMock);

    	Element iq          = new DOMElement("iq");
    	Element pubsub      = iq.addElement("pubsub");
    	pubsub.addAttribute("xmlns", JabberPubsub.NS_PUBSUB_OWNER);
    	Element configure   = pubsub.addElement("configure");
    	Element x           = configure.addElement("x");
    	Element field       = x.addElement("field");
    	field.addAttribute("var", Mock.FIELD_NAME);
    	Element value       = field.addElement("value");
    	value.addText(Mock.DEFAULT_VALUE);
    	IQ request          = new IQ(iq);
    	
    	parser.parse(request);
        
    	Assert.assertEquals(1, parser.getValues().size());
    	Assert.assertFalse(parser.isValid());
    }
    
    @Test(expected=NodeConfigurationException.class)
    public void testThrowsExceptionIfNodeValueNotSet() throws Exception {
    	Helper parser = new Helper(channelManager);
    	parser.getValues();
    }
    
    @Test(expected=NodeConfigurationException.class)
    public void testThrowsExceptionIfEmptyNodeValueProvided() throws Exception {
    	Helper parser = new Helper(channelManager);
    	parser.setNode("");
    	parser.getValues();
    }
    
    @Test
    public void testOverwritesChannelTypeIfUserAttemptsToChange() throws Exception {
    	
    	Element iq          = new DOMElement("iq");
    	Element pubsub      = iq.addElement("pubsub");
    	pubsub.addAttribute("xmlns", JabberPubsub.NS_PUBSUB_OWNER);
    	Element configure   = pubsub.addElement("configure");
    	Element x           = configure.addElement("x");
    	Element field       = x.addElement("field");
    	field.addAttribute("var", ChannelType.FIELD_NAME);
    	Element value       = field.addElement("value");
    	value.addText(ChannelType.DEFAULT_VALUE);
    	IQ request          = new IQ(iq);
    	
    	parser.parse(request);
    	
    	HashMap<String, String> configuration = new HashMap<String, String>();
    	String channelType = "mine-all-mine";
    	configuration.put(ChannelType.FIELD_NAME, channelType);
    	Mockito.when(channelManager.getNodeConf(Mockito.eq(node))).thenReturn(configuration);
    	
    	Assert.assertEquals(channelType, parser.getValues().get(ChannelType.FIELD_NAME));
    }
    
    @Test
    public void testAllowsSettingOfChannelTypeIfNotCurrentlySet() throws Exception {
    	Element iq          = new DOMElement("iq");
    	Element pubsub      = iq.addElement("pubsub");
    	pubsub.addAttribute("xmlns", JabberPubsub.NS_PUBSUB_OWNER);
    	Element configure   = pubsub.addElement("configure");
    	Element x           = configure.addElement("x");
    	Element field       = x.addElement("field");
    	field.addAttribute("var", ChannelType.FIELD_NAME);
    	Element value       = field.addElement("value");
    	value.addText(ChannelType.DEFAULT_VALUE);
    	IQ request          = new IQ(iq);
    	
    	parser.parse(request);
    	
    	HashMap<String, String> configuration = new HashMap<String, String>();
    	Mockito.when(channelManager.getNodeConf(Mockito.eq(node))).thenReturn(configuration);
    	
    	Assert.assertEquals(ChannelType.DEFAULT_VALUE, parser.getValues().get(ChannelType.FIELD_NAME));
    }
    
}