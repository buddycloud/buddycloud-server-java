package org.buddycloud.channelserver.channel.node.configuration;

import java.util.HashMap;

import org.buddycloud.channelserver.channel.node.configuration.field.ChannelTitle;
import org.buddycloud.channelserver.channel.node.configuration.field.ConfigurationFieldException;
import org.buddycloud.channelserver.channel.node.configuration.field.Field;
import org.buddycloud.channelserver.channel.node.configuration.field.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
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

public class HelperTest extends IQTestHandler
{
	private Helper parser;
	
	@Rule public ExpectedException thrown = ExpectedException.none();
	
	@Before
	public void setUp()
	{
		parser = new Helper();
	}
    
    @Test 
    public void testPassingPacketWhichDoesntContainConfigureElementThrowsException() 
	{
		Element iq     = new DOMElement("iq");
		iq.addElement("pubsub");
		IQ request = new IQ(iq);

		try {
		    parser.parse(request);
		} catch (NodeConfigurationException e) {
			assertSame(NodeConfigurationException.class, e.getClass());
			return;
		}
		fail("Exception not thrown");
	}
    
    @Test
    public void testNotProvidingAnyConfigurationFieldsReturnsNoConfiguration()
    {
    	Element iq        = new DOMElement("iq");
    	Element pubsub    = iq.addElement("pubsub");
    	Element configure = pubsub.addElement("configure");
    	configure.addElement("x");
    	IQ request        = new IQ(iq);
    	
    	parser.parse(request);
    	assertEquals(0, parser.getValues().size());
    }

    @Test
    public void testBadFieldConfigurationValueThrowsException()
    {
    	Factory factoryMock = Mockito.mock(Factory.class);
    	Mockito.doThrow(new ConfigurationFieldException())
	        .when(factoryMock).create((Node) Mockito.any());
    	parser.setFieldFactory(factoryMock);
    	
    	Element iq          = new DOMElement("iq");
    	Element pubsub      = iq.addElement("pubsub");
    	Element configure   = pubsub.addElement("configure");
    	Element x           = configure.addElement("x");
    	Element field       = x.addElement("field");
    	field.addAttribute("var", ChannelTitle.FIELD_NAME);
    	IQ request          = new IQ(iq);
    	
    	try {
    	    parser.parse(request); 
    	} catch (Exception e) {
    		assertSame(NodeConfigurationException.class, e.getClass());
    		return;
    	}
    	fail("Exception not thrown");
    }
    
    @Test
    public void testPassingSimpleConfigurationReturnsExceptedResults()
    {
    	Mock fieldMock = new Mock();
    	fieldMock.setValue(Mock.DEFAULT_VALUE);
    	Mock fieldMock2 = new Mock("MOCK_FIELD_TWO");
    	fieldMock2.setValue("My field value");
    	Factory factoryMock = Mockito.mock(Factory.class);
		Mockito
	        .when(factoryMock.create((Node) Mockito.anyObject()))
	    	.thenReturn(fieldMock, fieldMock2);
    	parser.setFieldFactory(factoryMock);

    	Element iq          = new DOMElement("iq");
    	Element pubsub      = iq.addElement("pubsub");
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
        
    	assertEquals(2, parser.getValues().size());
    	assertEquals(
    	    Mock.DEFAULT_VALUE,
    	    parser.getValues().get(fieldMock.getName())
    	);
    	assertEquals(
    		"My field value",
    		parser.getValues().get(fieldMock2.getName())
    	);
    }
    
    @Test
    public void testAllValidElementsMeansPositiveIsValidCall()
    {
    	Mock fieldMock = Mockito.mock(Mock.class);
    	Mockito
    	    .doReturn(true)
    	    .when(fieldMock)
    	    .isValid();
    	Factory factoryMock = Mockito.mock(Factory.class);
		Mockito
	        .when(factoryMock.create((Node) Mockito.anyObject()))
	    	.thenReturn(fieldMock);
    	parser.setFieldFactory(factoryMock);

    	Element iq          = new DOMElement("iq");
    	Element pubsub      = iq.addElement("pubsub");
    	Element configure   = pubsub.addElement("configure");
    	Element x           = configure.addElement("x");
    	Element field       = x.addElement("field");
    	field.addAttribute("var", Mock.FIELD_NAME);
    	Element value       = field.addElement("value");
    	value.addText(Mock.DEFAULT_VALUE);
    	IQ request          = new IQ(iq);
    	
    	parser.parse(request);
        
    	assertEquals(1, parser.getValues().size());
    	assertTrue(parser.isValid());
    }
    
    @Test
    public void testInvalidElementMeansNegativeIsValidCall()
    {
    	Mock fieldMock = Mockito.mock(Mock.class);
    	Mockito
    	    .doReturn(false)
    	    .when(fieldMock)
    	    .isValid();
    	Factory factoryMock = Mockito.mock(Factory.class);
		Mockito
	        .when(factoryMock.create((Node) Mockito.anyObject()))
	    	.thenReturn(fieldMock);
    	parser.setFieldFactory(factoryMock);

    	Element iq          = new DOMElement("iq");
    	Element pubsub      = iq.addElement("pubsub");
    	Element configure   = pubsub.addElement("configure");
    	Element x           = configure.addElement("x");
    	Element field       = x.addElement("field");
    	field.addAttribute("var", Mock.FIELD_NAME);
    	Element value       = field.addElement("value");
    	value.addText(Mock.DEFAULT_VALUE);
    	IQ request          = new IQ(iq);
    	
    	parser.parse(request);
        
    	assertEquals(1, parser.getValues().size());
    	assertFalse(parser.isValid());
    }
}