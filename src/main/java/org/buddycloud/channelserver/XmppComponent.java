package org.buddycloud.channelserver;

import org.buddycloud.channelserver.utils.ComponentBase;
import org.logicalcobwebs.proxool.ProxoolException;
import org.logicalcobwebs.proxool.configuration.PropertyConfigurator;

public class XmppComponent extends ComponentBase {

  public XmppComponent(Configuration configuration, String domain) throws ProxoolException {
    initialize(configuration, domain);
    engine = new ChannelsEngine(configuration);
    PropertyConfigurator.configure(configuration);
  }

}
