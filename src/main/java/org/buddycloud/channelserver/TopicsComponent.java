package org.buddycloud.channelserver;

import org.buddycloud.channelserver.utils.ComponentBase;
import org.jivesoftware.whack.ExternalComponentManager;
import org.xmpp.component.ComponentException;

public class TopicsComponent extends ComponentBase {

    public TopicsComponent(Configuration configuration, String domain) {
      initialize(configuration, domain);
      new TopicsEngine(configuration);
    }
}
