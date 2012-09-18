package org.buddycloud.channelserver.utils.node;

import java.security.InvalidParameterException;

import junit.framework.TestCase;

import org.buddycloud.channelserver.channel.node.configuration.NodeConfigurationException;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.Test;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Type;
import org.xmpp.packet.PacketError.Condition;

public class NodeViewAclTest extends TestCase {

	NodeViewAcl acl = new NodeViewAcl();
	String node = "/user/romeo@shakespeare.lit/posts";

	@Test
	public void testPassingInvalidAccessModelThrowsException() {
		try {
			acl.canViewNode(node, Affiliations.member.toString(),
					Subscriptions.none.toString(), "invalid-access-model");
		} catch (Exception e) {
			assertSame(InvalidParameterException.class, e.getClass());
			return;
		}
	}

	@Test
	public void testOutcastCanNotViewOpenChannelUnderAnySubscriptionType() {
		checkForBlockedAccess(Affiliations.outcast, Subscriptions.none,
				AccessModels.open, null, PacketError.Type.auth,
				PacketError.Condition.forbidden);
		checkForBlockedAccess(Affiliations.outcast, Subscriptions.pending,
				AccessModels.open, null, PacketError.Type.auth,
				PacketError.Condition.forbidden);
		checkForBlockedAccess(Affiliations.outcast, Subscriptions.unconfigured,
				AccessModels.open, null, PacketError.Type.auth,
				PacketError.Condition.forbidden);
		checkForBlockedAccess(Affiliations.outcast, Subscriptions.subscribed,
				AccessModels.open, null, PacketError.Type.auth,
				PacketError.Condition.forbidden);
	}

	@Test
	public void testOutcastCanNotViewPrivateChannelUnderAnySubscriptionType() {
		checkForBlockedAccess(Affiliations.outcast, Subscriptions.none,
				AccessModels.authorize, null, PacketError.Type.auth,
				PacketError.Condition.forbidden);
		checkForBlockedAccess(Affiliations.outcast, Subscriptions.pending,
				AccessModels.authorize, null, PacketError.Type.auth,
				PacketError.Condition.forbidden);
		checkForBlockedAccess(Affiliations.outcast, Subscriptions.unconfigured,
				AccessModels.authorize, null, PacketError.Type.auth,
				PacketError.Condition.forbidden);
		checkForBlockedAccess(Affiliations.outcast, Subscriptions.subscribed,
				AccessModels.authorize, null, PacketError.Type.auth,
				PacketError.Condition.forbidden);
	}

	@Test
	public void testOpenChannelWithNoSubscriptionCanBeViewedInAnyStateExcludingOutcast() {
		checkForAllowedAccess(Affiliations.owner, Subscriptions.none,
				AccessModels.open);
		checkForAllowedAccess(Affiliations.moderator, Subscriptions.none,
				AccessModels.open);
		checkForAllowedAccess(Affiliations.member, Subscriptions.none,
				AccessModels.open);
		checkForAllowedAccess(Affiliations.publisher, Subscriptions.none,
				AccessModels.open);
		checkForAllowedAccess(Affiliations.none, Subscriptions.none,
				AccessModels.open);
	}

	@Test
	public void testOpenChannelWithPendingSubscriptionCanBeViewedInAnyStateExcludingOutcast() {
		checkForAllowedAccess(Affiliations.owner, Subscriptions.pending,
				AccessModels.open);
		checkForAllowedAccess(Affiliations.moderator, Subscriptions.pending,
				AccessModels.open);
		checkForAllowedAccess(Affiliations.member, Subscriptions.pending,
				AccessModels.open);
		checkForAllowedAccess(Affiliations.publisher, Subscriptions.pending,
				AccessModels.open);
		checkForAllowedAccess(Affiliations.none, Subscriptions.pending,
				AccessModels.open);
	}

	@Test
	public void testOpenChannelWithUnconfiguredSubscriptionCanBeViewedInAnyStateExcludingOutcast() {
		checkForAllowedAccess(Affiliations.owner, Subscriptions.unconfigured,
				AccessModels.open);
		checkForAllowedAccess(Affiliations.moderator,
				Subscriptions.unconfigured, AccessModels.open);
		checkForAllowedAccess(Affiliations.member, Subscriptions.unconfigured,
				AccessModels.open);
		checkForAllowedAccess(Affiliations.publisher,
				Subscriptions.unconfigured, AccessModels.open);
		checkForAllowedAccess(Affiliations.none, Subscriptions.unconfigured,
				AccessModels.open);
	}

	@Test
	public void testOpenChannelWithSubscriptionCanBeViewedInAnyStateExcludingOutcast() {
		checkForAllowedAccess(Affiliations.owner, Subscriptions.subscribed,
				AccessModels.open);
		checkForAllowedAccess(Affiliations.moderator, Subscriptions.subscribed,
				AccessModels.open);
		checkForAllowedAccess(Affiliations.member, Subscriptions.subscribed,
				AccessModels.open);
		checkForAllowedAccess(Affiliations.publisher, Subscriptions.subscribed,
				AccessModels.open);
		checkForAllowedAccess(Affiliations.none, Subscriptions.subscribed,
				AccessModels.open);
	}

	@Test
	public void testPrivateNodeWithNoSubscriptionCanNotBeViewedByAnyone() {
		checkForBlockedAccess(Affiliations.owner, Subscriptions.none,
				AccessModels.authorize, NodeViewAcl.CLOSED_NODE,
				PacketError.Type.auth, PacketError.Condition.forbidden);
		checkForBlockedAccess(Affiliations.moderator, Subscriptions.none,
				AccessModels.authorize, NodeViewAcl.CLOSED_NODE,
				PacketError.Type.auth, PacketError.Condition.forbidden);
		checkForBlockedAccess(Affiliations.member, Subscriptions.none,
				AccessModels.authorize, NodeViewAcl.CLOSED_NODE,
				PacketError.Type.auth, PacketError.Condition.forbidden);
		checkForBlockedAccess(Affiliations.publisher, Subscriptions.none,
				AccessModels.authorize, NodeViewAcl.CLOSED_NODE,
				PacketError.Type.auth, PacketError.Condition.forbidden);
		checkForBlockedAccess(Affiliations.none, Subscriptions.none,
				AccessModels.authorize, NodeViewAcl.CLOSED_NODE,
				PacketError.Type.auth, PacketError.Condition.forbidden);
	}

	@Test
	public void testPrivateNodeWithPendingSubscriptionCanNotBeViewedByAnyone() {
		checkForBlockedAccess(Affiliations.owner, Subscriptions.pending,
				AccessModels.authorize, NodeViewAcl.PENDING_SUBSCRIPTION,
				PacketError.Type.auth, PacketError.Condition.not_authorized);
		checkForBlockedAccess(Affiliations.moderator, Subscriptions.pending,
				AccessModels.authorize, NodeViewAcl.PENDING_SUBSCRIPTION,
				PacketError.Type.auth, PacketError.Condition.not_authorized);
		checkForBlockedAccess(Affiliations.member, Subscriptions.pending,
				AccessModels.authorize, NodeViewAcl.PENDING_SUBSCRIPTION,
				PacketError.Type.auth, PacketError.Condition.not_authorized);
		checkForBlockedAccess(Affiliations.publisher, Subscriptions.pending,
				AccessModels.authorize, NodeViewAcl.PENDING_SUBSCRIPTION,
				PacketError.Type.auth, PacketError.Condition.not_authorized);
		checkForBlockedAccess(Affiliations.none, Subscriptions.pending,
				AccessModels.authorize, NodeViewAcl.PENDING_SUBSCRIPTION,
				PacketError.Type.auth, PacketError.Condition.not_authorized);
	}

	@Test
	public void testPrivateNodeWithUnconfiguredSubscriptionCanNotBeViewedByAnyone() {
		checkForBlockedAccess(Affiliations.owner, Subscriptions.unconfigured,
				AccessModels.authorize, NodeViewAcl.CONFIGURATION_REQUIRED,
				PacketError.Type.auth, PacketError.Condition.not_authorized);
		checkForBlockedAccess(Affiliations.moderator,
				Subscriptions.unconfigured, AccessModels.authorize,
				NodeViewAcl.CONFIGURATION_REQUIRED, PacketError.Type.auth,
				PacketError.Condition.not_authorized);
		checkForBlockedAccess(Affiliations.member, Subscriptions.unconfigured,
				AccessModels.authorize, NodeViewAcl.CONFIGURATION_REQUIRED,
				PacketError.Type.auth, PacketError.Condition.not_authorized);
		checkForBlockedAccess(Affiliations.publisher,
				Subscriptions.unconfigured, AccessModels.authorize,
				NodeViewAcl.CONFIGURATION_REQUIRED, PacketError.Type.auth,
				PacketError.Condition.not_authorized);
		checkForBlockedAccess(Affiliations.none, Subscriptions.unconfigured,
				AccessModels.authorize, NodeViewAcl.CONFIGURATION_REQUIRED,
				PacketError.Type.auth, PacketError.Condition.not_authorized);
	}

	@Test
	public void testPrivateNodeWithSubscriptionAllowsPostsToBeRetrieved() {
		checkForAllowedAccess(Affiliations.owner, Subscriptions.subscribed,
				AccessModels.authorize);
		checkForAllowedAccess(Affiliations.moderator, Subscriptions.subscribed,
				AccessModels.authorize);
		checkForAllowedAccess(Affiliations.member, Subscriptions.subscribed,
				AccessModels.authorize);
		checkForAllowedAccess(Affiliations.publisher, Subscriptions.subscribed,
				AccessModels.authorize);
	}

	@Test
	public void testNoAffiliationOnSubscribedNodeRefusesItemRetrieval() {
		checkForBlockedAccess(Affiliations.none, Subscriptions.subscribed,
				AccessModels.authorize, NodeViewAcl.CLOSED_NODE,
				PacketError.Type.auth, PacketError.Condition.not_authorized);
	}

	private void checkForBlockedAccess(Affiliations affiliation,
			Subscriptions subscription, AccessModels accessModel,
			String additionalError, Type type, Condition condition) {
		assertFalse(acl.canViewNode(node, affiliation.toString(),
				subscription.toString(), accessModel.toString()));
		assertEquals(type, acl.getReason().getType());
		assertEquals(condition, acl.getReason().getCondition());
		assertEquals(additionalError, acl.getReason()
				.getAdditionalErrorElement());
	}

	private void checkForAllowedAccess(Affiliations affiliation,
			Subscriptions subscription, AccessModels accessModel) {
		assertTrue(acl.canViewNode(node, affiliation.toString(),
				subscription.toString(), accessModel.toString()));
		assertNull(acl.getReason());
	}
}