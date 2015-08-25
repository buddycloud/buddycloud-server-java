package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.BlockingQueue;

import org.apache.commons.lang.StringUtils;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.QName;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public abstract class PacketProcessorAbstract implements PacketProcessor<IQ> {

  protected BlockingQueue<Packet> outQueue;
  protected ChannelManager channelManager;
  protected List<PubSubElementProcessor> elementProcessors = new LinkedList<PubSubElementProcessor>();

  protected Element pubsub;
  protected JID actor;

  protected void validateActor(IQ reqIQ) throws InterruptedException, IllegalActorException {

    if (pubsub.elementText(XMLConstants.ACTOR_ELEM) == null) {
      return;
    }
    actor = new JID(pubsub.elementText(XMLConstants.ACTOR_ELEM).trim());

    if (StringUtils.endsWithIgnoreCase(reqIQ.getFrom().getDomain(), "." + actor.getDomain())) {
      return;
    }
    throw new IllegalActorException();
  }

  @Override
  public void process(IQ reqIQ) throws Exception {

      pubsub = reqIQ.getChildElement();
      try {
        validateActor(reqIQ);
      } catch (IllegalActorException e) {
        sendPolicyViolationResponse(reqIQ);
        return;
      }

      // Let's get the possible rsm element
      Element rsm = pubsub.element(new QName("set", new Namespace("", "http://jabber.org/protocol/rsm")));

      @SuppressWarnings("unchecked")
      List<Element> elements = pubsub.elements();

      boolean handled = false;
      for (Element x : elements) {
          for (PubSubElementProcessor elementProcessor : elementProcessors) {
              if (elementProcessor.accept(x)) {
                  elementProcessor.process(x, actor, reqIQ, rsm);
                  handled = true;
              }
          }
      }

      if (!handled) {
          sendFeatureNotImplementedResponse(reqIQ);
      }
  }
  
  public void purgeElementProcessors() {
    elementProcessors.clear();
  }
  
  public void addElementProcessor(PubSubElementProcessor processor) {
    elementProcessors.add(processor);
  }

  private void sendFeatureNotImplementedResponse(IQ reqIQ) throws InterruptedException {
    IQ reply = IQ.createResultIQ(reqIQ);
    reply.setChildElement(reqIQ.getChildElement().createCopy());
    reply.setType(Type.error);
    PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.feature_not_implemented, org.xmpp.packet.PacketError.Type.cancel);
    reply.setError(pe);
    outQueue.put(reply);
  }
  
  private void sendPolicyViolationResponse(IQ reqIQ) throws InterruptedException {
    IQ reply = IQ.createResultIQ(reqIQ);
    reply.setChildElement(reqIQ.getChildElement().createCopy());
    reply.setType(Type.error);

    Element standardError = new DOMElement(XMLConstants.POLICY_VIOLATION, new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));
    Element extraError = new DOMElement(XMLConstants.INVALID_NODE, new org.dom4j.Namespace("", Buddycloud.NS_ERROR));
    Element error = new DOMElement(XMLConstants.ERROR_ELEM);
    error.addAttribute(XMLConstants.TYPE_ATTR, PacketError.Type.cancel.toXMPP());
    error.add(standardError);
    error.add(extraError);
    reply.setChildElement(error);
    outQueue.put(reply);
  }
  
}
