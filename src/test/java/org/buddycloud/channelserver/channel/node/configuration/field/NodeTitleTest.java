package org.buddycloud.channelserver.channel.node.configuration.field;

import junit.framework.Assert;

import org.apache.commons.lang.StringUtils;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.junit.Before;
import org.junit.Test;

public class NodeTitleTest extends IQTestHandler {

    private NodeTitle field;

    @Before
    public void setUp() {
        field = new NodeTitle();
    }

    @Test
    public void longestTitleIs128Characters() throws NodeStoreException {

        String testTitle = StringUtils.repeat("Doc, you built a time machine? Out of a delorean?!", NodeTitle.MAX_TITLE_LENGTH);
        field.setValue(testTitle);
        Assert.assertEquals(NodeTitle.MAX_TITLE_LENGTH, field.getValue().length());
    }

    @Test
    public void doesNotTrucateShorterStrings() {
        String testTitle = "Hi, my name's George, George McFly";
        field.setValue(testTitle);
        Assert.assertEquals(testTitle, field.getValue());
    }
}
