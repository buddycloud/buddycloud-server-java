package org.buddycloud.channelserver.utils.node;

public class NodeViewAcl {

	private NodeAclRefuseReason reasonForRefusal;

	public boolean canViewNode(String node, String affilliation,
			String subscription, String accessModel) {
		reasonForRefusal = null;
		return false;
	}

	public NodeAclRefuseReason getReason() {
		return reasonForRefusal;
	}
}
