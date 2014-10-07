package org.buddycloud.channelserver.channel;

import junit.framework.Assert;
import net.xeoh.plugins.base.PluginManager;
import net.xeoh.plugins.base.options.getplugin.OptionCapabilities;

import org.buddycloud.channelserver.channel.node.configuration.field.ContentType;
import org.buddycloud.channelserver.channel.validate.AtomEntry;
import org.buddycloud.channelserver.channel.validate.PayloadValidator;
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

        Mockito.when(channelManager.getNodeConfValue(node, ContentType.FIELD_NAME)).thenReturn(Atom.NS);

        validator = new ValidatePayload(channelManager, node);
    }

    @Test
    public void whenContentTypeIsAtomNsThenAtomValidatorReturned() throws Exception {
        Assert.assertTrue(validator.getValidator() instanceof AtomEntry);
    }

    @Test
    public void nullContentTypeFieldRetunsAtomValidator() throws Exception {
        Mockito.when(channelManager.getNodeConfValue(node, ContentType.FIELD_NAME)).thenReturn(null);
        Assert.assertTrue(validator.getValidator() instanceof AtomEntry);
    }

    @Test(expected = UnknownContentTypeException.class)
    public void unknownContentTypeThrowsException() throws Exception {
        Mockito.when(channelManager.getNodeConfValue(node, ContentType.FIELD_NAME)).thenReturn(Atom.NS_THREAD);
        validator.getValidator();
    }

    @Test(expected = NodeStoreException.class)
    public void throwsNodeStoreException() throws Exception {
        Mockito.when(channelManager.getNodeConfValue(node, ContentType.FIELD_NAME)).thenThrow(new NodeStoreException());
        validator.getValidator();
    }

    @Test(expected = UnknownContentTypeException.class)
    public void looksForForAppropriateValidatorInPlugins() throws Exception {
        Mockito.when(channelManager.getNodeConfValue(node, ContentType.FIELD_NAME)).thenReturn("any-strange-content-type");

        PluginManager pm = Mockito.mock(PluginManager.class);
        OptionCapabilities capabilities = Mockito.mock(OptionCapabilities.class);

        Mockito.when(capabilities.getCapabilities()).thenReturn(new String[] {"any-strange-content-type"});

        validator.setPluginManager(pm);
        validator.getValidator();

        Mockito.verify(pm, Mockito.times(1)).getPlugin(PayloadValidator.class, capabilities);
    }
}
