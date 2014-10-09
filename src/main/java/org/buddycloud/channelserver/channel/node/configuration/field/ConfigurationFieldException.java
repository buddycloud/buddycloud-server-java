package org.buddycloud.channelserver.channel.node.configuration.field;

public class ConfigurationFieldException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    public ConfigurationFieldException() {
        super();
    }

    public ConfigurationFieldException(String message) {
        super(message);
    }
}
