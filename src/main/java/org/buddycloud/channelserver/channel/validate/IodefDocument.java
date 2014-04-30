package org.buddycloud.channelserver.channel.validate;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.utils.node.item.payload.Iodef;
import org.dom4j.Element;
import org.xmpp.packet.JID;

/**
 * Validates IODEF Document payloads
 *
 * Only provides partial coverage of the IODEF standard at present
 */
public class IodefDocument implements PayloadValidator {

    private static final String ITEM_ID_SEPARATOR = "|";
    private static final String MISSING_CONTACT_EMAIL_TEXT = "contact-email-element-must-not-be-empty";
    private static final String MISSING_CONTACT_TYPE_ATTRIBUTE = "contact-type-attribute-required";
    private static final String MISSING_CONTACT_ROLE_ATTRIBUTE = "contact-role-attribute-required";
    private static final String MISSING_CONTACT_CHILD_ELEMENT = "contact-child-element-required";
    private static final String MISSING_CONTACT_ELEMENT = "contact-element-required";
    private static final String MISSING_IMPACT_ELEMENT = "impact-element-required";
    private static final String MISSING_IMPACT_TYPE_ATTRIBUTE = "impact-type-attribute-required";
    private static final String MISSING_IMPACT_LANG_ATTRIBUTE = "impact-lang-attribute-required";
    private static final String MISSING_ASSESSMENT_ELEMENT = "assessment-element-required";
    private static final String MISSING_INCIDENT_ID_NAME_ATTRIBUTE = "incident-id-name-attribute-required";
    private static final String MISSING_INCIDENT_ID_ELEMENT = "incident-id-element-required";
    private static final String MISSING_INCIDENT_ELEMENT = "incident-element-required";
    private static final String IODEF_MISSING_LANG_ATTRIBUTE = "iodef-lang-attribute-required";
    private static final String VERSION_NOT_SUPPORTED = "iodef-version-not-supported";
    private static final String MISSING_VERSION_ATTRIBUTE = "iodef-version-attribute-required";
    private static final String MISSING_IODEF_DOCUMENT_ELEMENT = "iodef-document-element-required";
    private static final String SUPPORTED_VERSION = "1.00";

    private static Logger LOGGER = Logger.getLogger(IodefDocument.class);

    private Element payload;

    private String errorMessage = "";

    private JID jid;
    private String channelServerDomain;
    private String node;
    private ChannelManager channelManager;

    Map<String, String> params = new HashMap<String, String>();

    public IodefDocument() {
    }

    public IodefDocument(Element payload) {
        setPayload(payload);
    }

    @Override
    public void setPayload(Element payload) {
        this.payload = payload;
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
        return isIodefValid(this.payload);
    }

    private boolean isIncidentValid(Element incident) {
        if (null == incident) {
            this.errorMessage = MISSING_INCIDENT_ELEMENT;
            return false;
        }

        if (!isIncidentIdValid(incident.element("IncidentID"))) {
            return false;
        }

        if (!isReportTimeValid(incident.element("ReportTime"))) {
            setReportTime(incident);
        }

        if (!isAssessmentValid(incident.element("Assessment"))) {
            return false;
        }

        if (!isContactValid(incident.element("Contact"))) {
            return false;
        }

        return true;
    }

    private void setReportTime(Element incident) {
        if (null == incident.element("ReportTime")) {
            incident.addElement("ReportTime");
        }
        LOGGER.debug("ReportTime was missing so we set it to now");
        incident.element("ReportTime").setText(Conf.formatDate(new Date()));
    }

    private boolean isContactValid(Element contact) {
        if (null == contact) {
            this.errorMessage = MISSING_CONTACT_ELEMENT;
            return false;
        }

        if (null == contact.attribute("role")
                || contact.attribute("role").getValue().isEmpty()) {
            this.errorMessage = MISSING_CONTACT_ROLE_ATTRIBUTE;
            return false;
        }

        if (null == contact.attribute("type")
                || contact.attribute("type").getValue().isEmpty()) {
            this.errorMessage = MISSING_CONTACT_TYPE_ATTRIBUTE;
            return false;
        }

        if (0 == contact.elements().size()) {
            this.errorMessage = MISSING_CONTACT_CHILD_ELEMENT;
            return false;
        }

        if (!isEmailValid(contact.element("Email"))) {
            return false;
        }

        return true;
    }

    private boolean isEmailValid(Element email) {
        if (null == email) {
            return true;
        }

        if (email.getTextTrim().isEmpty()) {
            this.errorMessage = MISSING_CONTACT_EMAIL_TEXT;
            return false;
        }

        return true;
    }

    private boolean isAssessmentValid(Element assessment) {
        if (null == assessment) {
            this.errorMessage = MISSING_ASSESSMENT_ELEMENT;
            return false;
        }

        if (!hasImpactClass(assessment)) {
            this.errorMessage = MISSING_IMPACT_ELEMENT;
            return false;
        }

        if (!isImpactValid(assessment.element("Impact"))) {
            return false;
        }

        return true;
    }

    private boolean hasImpactClass(Element assessment) {
        return null != assessment.element("Impact")
                || null != assessment.element("TimeImpact")
                || null != assessment.element("MonetaryImpact");
    }

    private boolean isImpactValid(Element impact) {
        if (null == impact) {
            return true;
        }

        if (null == impact.attribute("lang")
                || impact.attribute("lang").getValue().isEmpty()) {
            this.errorMessage = MISSING_IMPACT_LANG_ATTRIBUTE;
            return false;
        }

        if (null == impact.attribute("type")
                || impact.attribute("type").getValue().isEmpty()) {
            this.errorMessage = MISSING_IMPACT_TYPE_ATTRIBUTE;
            return false;
        }

        return true;
    }

    private boolean isReportTimeValid(Element reportTime) {
        if (null == reportTime || reportTime.getTextTrim().isEmpty()) {
            return false;
        }

        return true;
    }

    private boolean isIncidentIdValid(Element incidentId) {
        if (null == incidentId || incidentId.getTextTrim().isEmpty()) {
            this.errorMessage = MISSING_INCIDENT_ID_ELEMENT;
            return false;
        }

        if (null == incidentId.attribute("name")
                || incidentId.attribute("name").getValue().isEmpty()) {
            this.errorMessage = MISSING_INCIDENT_ID_NAME_ATTRIBUTE;
            return false;
        }

        return true;
    }

    private boolean isIodefValid(Element iodef) {
        if (iodef == null) {
            this.errorMessage = MISSING_IODEF_DOCUMENT_ELEMENT;
            return false;
        }

        if (null == iodef.attribute("version")) {
            this.errorMessage = MISSING_VERSION_ATTRIBUTE;
            return false;
        }

        if (!SUPPORTED_VERSION.equals(iodef.attribute("version").getValue())) {
            this.errorMessage = VERSION_NOT_SUPPORTED;
            return false;
        }

        if (null == iodef.attribute("lang")) {
            this.errorMessage = IODEF_MISSING_LANG_ATTRIBUTE;
            return false;
        }

        if (!isIncidentValid(iodef.element("Incident"))) {
            return false;
        }

        return true;
    }

    @Override
    public Element getPayload() {
        return this.payload;
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

    @Override
    public String getLocalItemId() {
        return GlobalItemIDImpl.toLocalId(getGlobalItemId());
    }

    @Override
    public String getGlobalItemId() {
        String globalItemId = null;
        Element incidentId = getIncidentId();
        if (null != incidentId && null != incidentId.attribute("name")) {
            String originator = incidentId.attribute("name").getValue();
            String originatorsIncidentId = incidentId.getTextTrim();

            String itemId = originator + ITEM_ID_SEPARATOR + originatorsIncidentId;

            globalItemId = new GlobalItemIDImpl(new JID(channelServerDomain),
                    node, itemId).toString();
        }
        return globalItemId;
    }

    @Override
    public String getInReplyTo() {
        // No replies for IODEF
        return null;
    }

    private Element getIncidentId() {
        if (null != this.payload.element("Incident")) {
            return this.payload.element("Incident").element("IncidentID");
        }
        return null;
    }

    @Override
    public boolean canValidate(String contentType) {
        return contentType.equals(Iodef.NS);
    }
}