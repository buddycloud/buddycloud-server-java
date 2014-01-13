package org.buddycloud.channelserver.channel;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.JID;

public class ValidateEntry {

	public static final String MISSING_CONTENT_ELEMENT = "content-element-required";
	public static final String MISSING_ENTRY_ELEMENT = "entry-element-required";
	public static final String UNSUPPORTED_CONTENT_TYPE = "unsupported-content-type";
	public static final String MAX_THREAD_DEPTH_EXCEEDED = "max-thread-depth-exceeded";
	public static final String PARENT_ITEM_NOT_FOUND = "parent-item-not-found";
	public static final String TARGETED_ITEM_NOT_FOUND = "targeted-item-not-found";
	public static final String IN_REPLY_TO_MISSING = "missing-in-reply-to";
	public static final String TARGET_ELEMENT_MISSING = "missing-target";
	public static final String MISSING_TARGET_ID = "missing-target-id";
	public static final String RATING_OUT_OF_RANGE = "rating-out-of-range";
	public static final String INVALID_RATING_VALUE = "invalid-rating";
	public static final String TARGET_MUST_BE_IN_SAME_THREAD = "target-outside-thread";

	public static final String CONTENT_TEXT = "text";
	public static final String CONTENT_XHTML = "xhtml";

	public static final String NS_ATOM = "http://www.w3.org/2005/Atom";
	public static final String NS_ACTIVITYSTREAM = "http://activitystrea.ms/spec/1.0/";
	public static final String NS_ATOM_THREAD = "http://purl.org/syndication/thread/1.0";
	public static final String NS_GEOLOCATION = "http://jabber.org/protocol/geoloc";
	public static final String NS_REVIEW = "http://activitystrea.ms/schema/1.0/review";

	public static final String AUTHOR_URI_PREFIX = "acct:";
	public static final String AUTHOR_TYPE = "person";

	public static final String POST_TYPE_NOTE = "note";
	public static final String POST_TYPE_COMMENT = "comment";

	public static final String ACTIVITY_VERB_POST = "post";

	private static Logger LOGGER = Logger.getLogger(ValidateEntry.class);

	private Element entry;

	private String errorMessage = "";
	private String inReplyTo = null;
	private String targetId = null;
	private int itemRating = 0;
	private Element meta;
	private Element media;

	private JID jid;
	private String channelServerDomain;
	private String node;
	private ChannelManager channelManager;
	private NodeItem replyingToItem;

	Map<String, String> params = new HashMap<String, String>();

	private Element geoloc;

	public ValidateEntry() {
	}

	public ValidateEntry(Element entry) {
		setEntry(entry);
	}

	public void setEntry(Element entry) {
		this.entry = entry;
	}

	public String getErrorMessage() {
		return this.errorMessage;
	}

	public void setChannelManager(ChannelManager channelManager) {
		this.channelManager = channelManager;
	}

	/**
	 * This is a big hackety-hack.
	 * 
	 * @throws InterruptedException
	 * @throws NodeStoreException
	 */
	public boolean isValid() throws NodeStoreException {
		if (this.entry == null) {
			this.errorMessage = MISSING_ENTRY_ELEMENT;
			return false;
		}

		Element id = this.entry.element("id");
		if ((id == null) || (true == id.getText().isEmpty())) {
			if (null != id) {
				id.detach();
			}
			LOGGER.debug("ID of the entry was missing. We add a default one to it: 1");
			this.entry.addElement("id").setText("1");
		}

		Element title = this.entry.element("title");
		if (null == title) {
			LOGGER.debug("Title of the entry was missing. We add a default one to it: 'Post'.");
			title = this.entry.addElement("title");
			title.setText("Post");
		}
		this.params.put("title", title.getText());

		Element content = this.entry.element("content");
		if (null == content) {
			this.errorMessage = MISSING_CONTENT_ELEMENT;
			return false;
		}
		String contentType = content.attributeValue("type");
		if (null == contentType) {
			contentType = CONTENT_TEXT;
		}
		if ((false == contentType.equals(CONTENT_TEXT))
				&& (false == contentType.equals(CONTENT_XHTML))) {
			this.errorMessage = UNSUPPORTED_CONTENT_TYPE;
			return false;
		}
		this.params.put("content", content.getText());
		this.params.put("content-type", contentType);

		Element updated = this.entry.element("updated");
		if (null == updated) {

			String updateTime = Conf.formatDate(new Date());
			LOGGER.debug("Update of the entry was missing. We add a default one to it: '"
					+ updateTime + "'.");
			this.entry.addElement("updated").setText(updateTime);
		}

		this.geoloc = this.entry.element("geoloc");

		if (false == validateInReplyToElement(this.entry.element("in-reply-to"))) {
			return false;
		}

		if (false == validateTargetElement(this.entry.element("target"))) {
			return false;
		}
		
		if (false == validateRatingElement(this.entry.element("rating"))) {
			return false;
		}

		Element meta = this.entry.element("meta");
		if (null != meta) {
			this.meta = meta;
		}

		Element media = this.entry.element("media");
		if (null != media) {
			this.media = media;
		}

		return true;
	}

	public Element getPayload() {

		Element entry = new DOMElement("entry", new org.dom4j.Namespace("",
				NS_ATOM));
		entry.add(new org.dom4j.Namespace("activity", NS_ACTIVITYSTREAM));

		String id = UUID.randomUUID().toString();
		String postType = POST_TYPE_NOTE;
		String activityVerb = ACTIVITY_VERB_POST;

		entry.addElement("id").setText(
				"tag:" + channelServerDomain + "," + node + "," + id);

		entry.addElement("title").setText(this.params.get("title"));

		Element content = entry.addElement("content");
		content.setText(this.params.get("content"));
		content.addAttribute("type", this.params.get("content-type"));

		String publishedDate = Conf.formatDate(new Date());

		entry.addElement("published").setText(publishedDate);

		entry.addElement("updated").setText(publishedDate);

		Element author = entry.addElement("author");

		author.addElement("name").setText(jid.toBareJID());

		author.addElement("uri").setText(AUTHOR_URI_PREFIX + jid.toBareJID());

		author.addElement("activity:object-type").setText(AUTHOR_TYPE);

		if (this.geoloc != null) {
			entry.add(this.geoloc.createCopy());
		}

		if (this.inReplyTo != null) {
			Element reply = entry.addElement("in-reply-to");
			reply.addNamespace("", NS_ATOM_THREAD);
			reply.addAttribute("ref", inReplyTo);
			postType = POST_TYPE_COMMENT;
		}

		this.geoloc = this.entry.element("geoloc");

		entry.addElement("activity:verb").setText(activityVerb);

		Element activityObject = entry.addElement("activity:object");
		activityObject.addElement("activity:object-type").setText(postType);

		if (null != meta) {
			entry.add(meta.createCopy());
		}

		if (null != media) {
			entry.add(media.createCopy());
		}

		if (null != targetId) {
			GlobalItemIDImpl globalTargetId = new GlobalItemIDImpl(new JID(
					channelServerDomain), node, targetId);
			Element target = entry.addElement("activity:target");
			target.addElement("id")
					.setText(globalTargetId.toString());
			target.addElement("activity:object-type").setText("post");
		}

		return entry;
	}

	public void setUser(JID jid) {
		this.jid = jid;
	}

	public void setNode(String node) {
		this.node = node;
	}

	public void setTo(String channelServerDomain) {
		this.channelServerDomain = channelServerDomain;
	}

	private boolean validateInReplyToElement(Element reply)
			throws NodeStoreException {
		if (null == reply) {
			return true;
		}

		inReplyTo = reply.attributeValue("ref");
		if (true == GlobalItemIDImpl.isGlobalId(inReplyTo)) {
			inReplyTo = GlobalItemIDImpl.toLocalId(inReplyTo);
		}

		replyingToItem = null;
		if (null == (replyingToItem = channelManager.getNodeItem(node,
				inReplyTo))) {
			this.errorMessage = PARENT_ITEM_NOT_FOUND;
			return false;
		}
		if (null != replyingToItem.getInReplyTo()) {
			LOGGER.error("User is attempting to reply to a reply");
			this.errorMessage = MAX_THREAD_DEPTH_EXCEEDED;
			return false;
		}
		return true;
	}

	private boolean validateTargetElement(Element target)
			throws NodeStoreException {
		if (null == target) {
			return true;
		}
		targetId = target.elementText("id");
		if ((null == targetId) || (0 == targetId.length())) {
			this.errorMessage = MISSING_TARGET_ID;
			return false;
		}
		if (null == inReplyTo) {
			this.errorMessage = IN_REPLY_TO_MISSING;
			return false;
		}
		if (true == GlobalItemIDImpl.isGlobalId(targetId)) {
			targetId = GlobalItemIDImpl.toLocalId(targetId);
		}
		NodeItem targetItem;
		if (true == targetId.equals(replyingToItem.getId())) {
			targetItem = replyingToItem;
		} else {
			targetItem = channelManager.getNodeItem(node, targetId);
		}
		if (null == targetItem) {
			this.errorMessage = TARGETED_ITEM_NOT_FOUND;
			return false;
		}
		if (true == targetItem.getId().equals(targetId)) {
			return true;
		}
		if ((null == targetItem.getInReplyTo())
				|| (false == targetItem.getInReplyTo().equals(targetId))) {
			this.errorMessage = TARGET_MUST_BE_IN_SAME_THREAD;
			return false;
		}
		return true;
	}
	
	private boolean validateRatingElement(Element rating) {
		if (null == rating) {
			return true;
		}
		if (null == inReplyTo) {
			this.errorMessage = IN_REPLY_TO_MISSING;
			return false;
		}
		if (null == targetId) {
			this.errorMessage = TARGET_ELEMENT_MISSING;
			return false;
		}
		try {
		    double itemRatingFloat = Double.parseDouble(rating.getTextTrim());
		    if (itemRatingFloat != Math.floor(itemRatingFloat)) {
		    	throw new NumberFormatException("Non-integer rating provided");
		    }
		    itemRating = (int) itemRatingFloat;
		} catch (NumberFormatException e) {
			this.errorMessage = INVALID_RATING_VALUE;
			return false;
		}
		if ((itemRating < 1) || (itemRating > 5)) {
			this.errorMessage = RATING_OUT_OF_RANGE;
			return false;
		}
		return true;
	}
}