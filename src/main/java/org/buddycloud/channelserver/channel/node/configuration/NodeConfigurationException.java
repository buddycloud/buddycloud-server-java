package org.buddycloud.channelserver.channel.node.configuration;

public class NodeConfigurationException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    public NodeConfigurationException() {}

    public NodeConfigurationException(String message) {
        super(message);
    }
}
