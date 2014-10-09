package org.buddycloud.channelserver.channel.validate;

import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.UUID;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.XMLConstants;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.utils.node.item.payload.ActivityStreams;
import org.buddycloud.channelserver.utils.node.item.payload.Atom;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.JID;

public class AtomEntry implements PayloadValidator {

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
    public static final String CAN_ONLY_RATE_A_POST = "invalid-rating-request";
    public static final String ITEM_ALREADY_RATED = "item-already-rated";

    public static final String CONTENT_TEXT = "text";
    public static final String CONTENT_XHTML = "xhtml";

    public static final String AUTHOR_URI_PREFIX = "acct:";
    public static final String AUTHOR_TYPE = "person";

    public static final String POST_TYPE_NOTE = "note";
    public static final String POST_TYPE_COMMENT = "comment";

    public static final String ACTIVITY_VERB_POST = "post";
    public static final String ACTIVITY_VERB_RATED = "rated";

    private static final Logger LOGGER = Logger.getLogger(Atom.class);

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
    private NodeItem targetItem;

    Map<String, String> params = new HashMap<String, String>();

    private Element geoloc;
    private String globalItemID;

    public AtomEntry() {

    }

    public AtomEntry(Element item) {
        setPayload(item);
    }

    @Override
    public void setPayload(Element item) {
        if (null != item) {
            this.entry = item.element("entry");
        }
    }

    @Override
    public String getErrorMessage() {
        return this.errorMessage;
    }

    @Override
    public void setChannelManager(ChannelManager channelManager) {
        this.channelManager = channelManager;
    }

    @Override
    public boolean isValid() throws NodeStoreException {
        if (this.entry == null) {
            this.errorMessage = MISSING_ENTRY_ELEMENT;
            return false;
        }

        Element id = this.entry.element(XMLConstants.ID_ELEM);
        if ((id == null) || (id.getText().isEmpty())) {
            if (null != id) {
                id.detach();
            }
            LOGGER.debug("ID of the entry was missing. We add a default one to it: 1");
            this.entry.addElement("id").setText("1");
        }

        Element title = this.entry.element(XMLConstants.TITLE_ELEM);
        if (null == title) {
            LOGGER.debug("Title of the entry was missing. We add a default one to it: 'Post'.");
            title = this.entry.addElement(XMLConstants.TITLE_ELEM);
            title.setText("Post");
        }
        this.params.put(XMLConstants.TITLE_ELEM, title.getText());

        Element content = this.entry.element("content");
        if (null == content) {
            this.errorMessage = MISSING_CONTENT_ELEMENT;
            return false;
        }
        String contentType = content.attributeValue(XMLConstants.TYPE_ATTR);
        if (null == contentType) {
            contentType = CONTENT_TEXT;
        }
        if ((!contentType.equals(CONTENT_TEXT)) && (!contentType.equals(CONTENT_XHTML))) {
            this.errorMessage = UNSUPPORTED_CONTENT_TYPE;
            return false;
        }
        this.params.put("content", content.getText());
        this.params.put("content-type", contentType);

        Element updated = this.entry.element(XMLConstants.UPDATED_ELEM);
        if (null == updated) {

            String updateTime = Conf.formatDate(new Date());
            LOGGER.debug("Update of the entry was missing. We add a default one to it: '" + updateTime + "'.");
            this.entry.addElement(XMLConstants.UPDATED_ELEM).setText(updateTime);
        }

        this.geoloc = this.entry.element(XMLConstants.GEOLOC_ELEM);

        if (!validateInReplyToElement(this.entry.element(XMLConstants.IN_REPLY_TO_ELEM))) {
            return false;
        }

        if (!validateTargetElement(this.entry.element(XMLConstants.TARGET_ELEM))) {
            return false;
        }

        if (!validateRatingElement(this.entry.element(XMLConstants.RATING_ELEM))) {
            return false;
        }

        Element meta = this.entry.element(XMLConstants.META_ELEM);
        if (null != meta) {
            this.meta = meta;
        }

        Element media = this.entry.element(XMLConstants.MEDIA_ELEM);
        if (null != media) {
            this.media = media;
        }

        return true;
    }

    @Override
    public Element getPayload() {

        Element entry = new DOMElement(XMLConstants.ENTRY_ELEM, new org.dom4j.Namespace("", Atom.NS));
        entry.add(new org.dom4j.Namespace("activity", ActivityStreams.NS));

        String postType = POST_TYPE_NOTE;
        String activityVerb = ACTIVITY_VERB_POST;

        entry.addElement(XMLConstants.ID_ELEM).setText(getGlobalItemId());

        String title = this.params.get("title");
        String itemContent = this.params.get("content");
        String publishedDate = Conf.formatDate(new Date());

        entry.addElement(XMLConstants.PUBLISHED_ELEM).setText(publishedDate);

        entry.addElement(XMLConstants.UPDATED_ELEM).setText(publishedDate);

        Element author = entry.addElement(XMLConstants.AUTHOR_ELEM);

        author.addElement(XMLConstants.NAME_ELEM).setText(jid.toBareJID());

        author.addElement(XMLConstants.URI_ELEM).setText(AUTHOR_URI_PREFIX + jid.toBareJID());

        author.addElement(XMLConstants.ACTIVITY_OBJECT_TYPE_ELEM).setText(AUTHOR_TYPE);

        if (this.geoloc != null) {
            entry.add(this.geoloc.createCopy());
        }

        if (this.inReplyTo != null) {
            Element reply = entry.addElement(XMLConstants.IN_REPLY_TO_ELEM);
            reply.addNamespace("", Atom.NS_THREAD);
            reply.addAttribute("ref", inReplyTo);
            postType = POST_TYPE_COMMENT;
        }

        this.geoloc = this.entry.element("geoloc");

        if (null != meta) {
            entry.add(meta.createCopy());
        }

        if (null != media) {
            entry.addNamespace(Buddycloud.NS_MEDIA_PREFIX, Buddycloud.NS_MEDIA);
            Element mediaElement = media.createCopy();
            for (Iterator<Element> iter = mediaElement.elements().iterator(); iter.hasNext();) {
                Element item = iter.next();
                item.setName(Buddycloud.NS_MEDIA_PREFIX + ":item");
            }
            mediaElement.setName(Buddycloud.NS_MEDIA_PREFIX + ":media");
            entry.add(mediaElement);
        }

        if (null != targetId) {
            GlobalItemIDImpl globalTargetId = new GlobalItemIDImpl(new JID(channelServerDomain), node, targetId);
            Element target = entry.addElement("activity:target");
            target.addElement("id").setText(globalTargetId.toString());
            target.addElement("activity:object-type").setText("post");
        }

        if (itemRating > 0) {
            entry.addNamespace("review", ActivityStreams.NS_REVIEW);
            String rating = String.format("%d.0", itemRating);
            entry.addElement("review:rating").setText(rating);
            title = "Rating";
            itemContent = "rating:" + rating;
            activityVerb = ACTIVITY_VERB_RATED;
        }

        entry.addElement("title").setText(title);

        Element content = entry.addElement("content");
        content.setText(itemContent);
        content.addAttribute("type", this.params.get("content-type"));

        entry.addElement("activity:verb").setText(activityVerb);
        Element activityObject = entry.addElement("activity:object");
        activityObject.addElement("activity:object-type").setText(postType);

        return entry;
    }

    @Override
    public void setUser(JID jid) {
        this.jid = jid;
    }

    @Override
    public void setNode(String node) {
        this.node = node;
    }

    @Override
    public void setTo(String channelServerDomain) {
        this.channelServerDomain = channelServerDomain;
    }

    private boolean validateInReplyToElement(Element reply) throws NodeStoreException {
        if (null == reply) {
            return true;
        }

        inReplyTo = reply.attributeValue("ref");
        if (GlobalItemIDImpl.isGlobalId(inReplyTo)) {
            inReplyTo = GlobalItemIDImpl.toLocalId(inReplyTo);
        }

        replyingToItem = channelManager.getNodeItem(node, inReplyTo);
        if (null == replyingToItem) {
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

    private boolean validateTargetElement(Element target) throws NodeStoreException {
        if (null == target) {
            return true;
        }
        targetId = target.elementText(XMLConstants.ID_ELEM);
        if ((null == targetId) || (0 == targetId.length())) {
            this.errorMessage = MISSING_TARGET_ID;
            return false;
        }
        if (null == inReplyTo) {
            this.errorMessage = IN_REPLY_TO_MISSING;
            return false;
        }
        if (GlobalItemIDImpl.isGlobalId(targetId)) {
            targetId = GlobalItemIDImpl.toLocalId(targetId);
        }
        if (targetId.equals(replyingToItem.getId())) {
            targetItem = replyingToItem;
        } else {
            targetItem = channelManager.getNodeItem(node, targetId);
        }
        if (null == targetItem) {
            this.errorMessage = TARGETED_ITEM_NOT_FOUND;
            return false;
        }
        if (targetItem.getId().equals(targetId)) {
            return true;
        }
        if ((null == targetItem.getInReplyTo()) || (!targetItem.getInReplyTo().equals(targetId))) {
            this.errorMessage = TARGET_MUST_BE_IN_SAME_THREAD;
            return false;
        }
        return true;
    }

    private boolean validateRatingElement(Element rating) throws NodeStoreException {
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
        if (targetItem.getPayload().indexOf(ActivityStreams.NS_REVIEW) > -1) {
            this.errorMessage = CAN_ONLY_RATE_A_POST;
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

        GlobalItemID globalTargetId = new GlobalItemIDImpl(new JID(channelServerDomain), node, targetId);
        if (channelManager.userHasRatedPost(node, jid, globalTargetId)) {
            this.errorMessage = ITEM_ALREADY_RATED;
            return false;
        }

        return true;
    }

    @Override
    public String getLocalItemId() {
        return GlobalItemIDImpl.toLocalId(getGlobalItemId());
    }

    @Override
    public String getGlobalItemId() {
        if (null == globalItemID) {
            String id = UUID.randomUUID().toString();
            globalItemID = new GlobalItemIDImpl(new JID(channelServerDomain), node, id).toString();
        }
        return globalItemID;
    }

    @Override
    public String getInReplyTo() {
        String localReply = null;
        Element inReplyToElement = this.entry.element(XMLConstants.IN_REPLY_TO_ELEM);
        if (null != inReplyToElement) {
            localReply = GlobalItemIDImpl.toLocalId(inReplyToElement.attributeValue("ref"));
        }
        return localReply;
    }

    @Override
    public String[] capabilities() {
        return new String[] {null, Atom.NS};
    }

    public boolean canValidate(String contentType) {
        return Arrays.asList(capabilities()).contains(contentType);
    }
}
