package org.buddycloud.channelserver;

import org.buddycloud.channelserver.utils.ComponentBase;

public class TopicsComponent extends ComponentBase {

    public TopicsComponent(Configuration configuration, String domain) {
      initialize(configuration, domain);
      this.engine = new TopicsEngine(configuration);
    }
}
