package org.buddycloud.channelserver.channel;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.apache.log4j.Logger;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.JID;

public class ValidateEntry {

	public static final String MISSING_CONTENT_ELEMENT = "content-element-required";
	public static final String MISSING_ENTRY_ELEMENT = "entry-element-required";
	public static final String UNSUPPORTED_CONTENT_TYPE = "unsupported-content-type";

	public static final String CONTENT_TEXT = "text";
	public static final String CONTENT_XHTML = "xhtml";

	public static final String NS_ATOM = "http://www.w3.org/2005/Atom";
	public static final String NS_ACTIVITYSTREAM = "http://activitystrea.ms/spec/1.0/";
	public static final String NS_ATOM_THREAD = "http://purl.org/syndication/thread/1.0";
	public static final String NS_GEOLOCATION = "http://jabber.org/protocol/geoloc";

	public static final String AUTHOR_URI_PREFIX = "acct:";
	public static final String AUTHOR_TYPE = "person";

	public static final String POST_TYPE_NOTE = "note";
	public static final String POST_TYPE_COMMENT = "comment";

	public static final String ACTIVITY_VERB_POST = "post";

	private static Logger LOGGER = Logger.getLogger(ValidateEntry.class);

	private Element entry;

	private String errorMessage = "";
	private String inReplyTo;
	private Element meta;
	private Element media;

	Map<String, String> params = new HashMap<String, String>();

	private Element geoloc;

	public ValidateEntry() {}
	
	public ValidateEntry(Element entry) {
		setEntry(entry);
	}
	
	public void setEntry(Element entry) {
		this.entry = entry;
	}

	public String getErrorMessage() {
		return this.errorMessage;
	}

	/**
	 * This is a big hackety-hack.
	 */
	public boolean isValid() {
		if (this.entry == null) {
			this.errorMessage = MISSING_ENTRY_ELEMENT;
			return false;
		}

		Element id = this.entry.element("id");
		if ((id == null) || (true == id.getText().isEmpty())) {
			if (null != id)
				id.detach();
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

		Element reply = this.entry.element("in-reply-to");
		if (null != reply) {
			inReplyTo = reply.attributeValue("ref");
			if (-1 != inReplyTo.indexOf(",")) {
				String[] tokens = inReplyTo.split(",");
				inReplyTo = tokens[2];
			}
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

	public Element getPayload(JID jid, String channelServerDomain,
			String node) {

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

		return entry;
	}

}