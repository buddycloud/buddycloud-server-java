package org.buddycloud.channelserver.pubsub.model;

import java.util.Date;

import org.xmpp.resultsetmanagement.Result;
import org.xmpp.resultsetmanagement.ResultSet;

public interface NodeThread extends Result {

    String getId();

    Date getUpdated();

    ResultSet<NodeItem> getItems();

}
