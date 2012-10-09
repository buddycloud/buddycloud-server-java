package org.buddycloud.channelserver.packetprocessor.iq;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.NodeStore;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.discoinfo.JabberDiscoInfo;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.register.JabberRegister;
import org.buddycloud.channelserver.queue.statemachine.IStatemachine;
import org.buddycloud.channelserver.queue.statemachine.StateMachineBuilder;

import org.apache.log4j.Logger;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class IQProcessor implements PacketProcessor<IQ> {

    private static final Logger LOGGER = Logger.getLogger(IQProcessor.class);
    
    private Map<String, PacketProcessor<IQ>> processorsPerNamespace = 
            new HashMap<String, PacketProcessor<IQ>>();
    private BlockingQueue<Packet> outQueue;
    private ChannelManager channelManager;
	
	public IQProcessor(BlockingQueue<Packet> outQueue, Properties conf, ChannelManager channelManager) {
		this.outQueue  = outQueue;
		this.channelManager = channelManager;
		
		JabberPubsub ps = new JabberPubsub(outQueue, conf, channelManager);
		
        processorsPerNamespace.put(JabberDiscoInfo.NAMESPACE_URI,
                new JabberDiscoInfo(outQueue, conf, channelManager));
        processorsPerNamespace.put(JabberRegister.NAMESPACE_URI, 
                new JabberRegister(outQueue, conf, channelManager));
        processorsPerNamespace.put(JabberPubsub.NAMESPACE_URI, ps);
        processorsPerNamespace.put(JabberPubsub.NS_PUBSUB_OWNER, ps);
	}
	
    @Override
    public void process(IQ packet) throws Exception {
        
        LOGGER.debug("Finding IQ processor for namespace " + packet.getChildElement().getNamespaceURI());

        PacketProcessor<IQ> namespaceProcessor = processorsPerNamespace.get(
                packet.getChildElement().getNamespaceURI());
        
        if (packet.getChildElement() != null
                && packet.getChildElement().getNamespaceURI() != null
                && namespaceProcessor != null) {
        	LOGGER.trace("Using namespace processor: " + namespaceProcessor.getClass().getName());
            namespaceProcessor.process(packet);
            return;

        }
        
        LOGGER.debug("Couldn't find processor for namespace " + packet.getChildElement().getNamespaceURI());

        if (packet.getType() == IQ.Type.set || packet.getType() == IQ.Type.get) {

            IQ reply = IQ.createResultIQ(packet);
            reply.setChildElement(packet.getChildElement().createCopy());
            reply.setType(Type.error);
            PacketError pe = new PacketError(
                    org.xmpp.packet.PacketError.Condition.service_unavailable,
                    org.xmpp.packet.PacketError.Type.cancel);
            reply.setError(pe);

            this.outQueue.put(reply);
            return;

        }
// TODO
        /*
        if (packet.getType() == IQ.Type.result || packet.getType() == IQ.Type.error) {
            // This might be a reply to a state we are on.
            Map<String, String> state = channelManager.getState(packet.getID());
            if (!state.isEmpty()) {
                IStatemachine sm = StateMachineBuilder.buildFromState(packet,
                        state, channelManager);
                outQueue.put(sm.nextStep());
                return;
            }
        }
        */

        LOGGER.error("Could not handle packet " + packet.toXML());
        
    }

}
