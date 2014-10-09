package org.buddycloud.channelserver.utils.node;

import org.xmpp.packet.PacketError.Condition;
import org.xmpp.packet.PacketError.Type;

public class NodeAclRefuseReason {

    private Type type;
    private Condition condition;
    private String additionalErrorElement;

    public NodeAclRefuseReason(Type type, Condition condition, String additionalErrorElement) {
        this.type = type;
        this.condition = condition;
        this.additionalErrorElement = additionalErrorElement;
    }

    public Type getType() {
        return type;
    }

    public Condition getCondition() {
        return condition;
    }

    public String getAdditionalErrorElement() {
        return additionalErrorElement;
    }
}
