package org.buddycloud.channelserver.utils.node;

import java.security.InvalidParameterException;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.xmpp.packet.PacketError;

public class NodeViewAcl {

	private static final Logger LOGGER = Logger.getLogger(NodeViewAcl.class);
	private static final String INVALID_ACCESS_MODEL = "Invalid access model";

	public static final String CLOSED_NODE = "closed-node";
	public static final String PENDING_SUBSCRIPTION = "pending-subscription";
	public static final String CONFIGURATION_REQUIRED = "configuration-required";

	private NodeAclRefuseReason reasonForRefusal;

	public boolean canViewNode(String node, Affiliations affilliation,
			Subscriptions subscription, AccessModels accessModel) {
		LOGGER.trace("Being asked for access to " + node + " with properties "
				+ affilliation + " :: " + subscription + " :: " + accessModel);
		reasonForRefusal = null;

		if (Affiliations.outcast.toString().equals(affilliation.toString())) {
			reasonForRefusal = new NodeAclRefuseReason(PacketError.Type.auth,
					PacketError.Condition.forbidden, null);
			return false;
		}

		if (accessModel.toString().equals(AccessModels.open.toString())) {
			return openChannelAcl(node, subscription, affilliation);
		} else if (accessModel.toString().equals(AccessModels.authorize.toString())) {
			return privateChannelAcl(node, subscription, affilliation);
		} else if (accessModel.toString().equals(AccessModels.whitelist.toString())) {
            return whitelistAcl(node, subscription, affilliation);
        }
		throw new InvalidParameterException(INVALID_ACCESS_MODEL);
	}

    private boolean whitelistAcl(String node, Subscriptions subscription, Affiliations affilliation) {
        LOGGER.trace("Whitelist not implemented, returning equivalent of authorized");
        return privateChannelAcl(node, subscription, affilliation);
    }

    private boolean privateChannelAcl(String node, Subscriptions subscription,
			Affiliations affilliation) {
		if (Subscriptions.none.toString().equals(subscription.toString())) {
			reasonForRefusal = new NodeAclRefuseReason(PacketError.Type.auth,
					PacketError.Condition.forbidden, CLOSED_NODE);
			return false;
		} else if (Subscriptions.pending.toString().equals(subscription.toString())) {
			reasonForRefusal = new NodeAclRefuseReason(PacketError.Type.auth,
					PacketError.Condition.not_authorized, PENDING_SUBSCRIPTION);
			return false;
		} else if (Subscriptions.unconfigured.toString().equals(subscription.toString())) {
			reasonForRefusal = new NodeAclRefuseReason(PacketError.Type.auth,
					PacketError.Condition.not_authorized,
					CONFIGURATION_REQUIRED);
			return false;
		} else if (Affiliations.none.toString().equals(affilliation.toString())) {
			reasonForRefusal = new NodeAclRefuseReason(PacketError.Type.auth,
					PacketError.Condition.not_authorized, CLOSED_NODE);
			return false;
		}
		return true;
	}

	private boolean openChannelAcl(String node, Subscriptions subscription,
			Affiliations affilliation) {
		return true;
	}

	public NodeAclRefuseReason getReason() {
		return reasonForRefusal;
	}
}
