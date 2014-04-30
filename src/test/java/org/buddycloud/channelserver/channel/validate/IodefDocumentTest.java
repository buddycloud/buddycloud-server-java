package org.buddycloud.channelserver.channel.validate;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.TestHandler;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.dom4j.Element;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class IodefDocumentTest extends TestHandler {

    private IodefDocument validator;

    private IQ publishRequest;
    private Element publishIodef;

    private ChannelManager channelManager;

    JID jid = new JID("juliet@shakespeare.lit/balcony");
    String node = "/users/romeo@shakespeare.lit/posts";
    String server = "channels.shakespeare.lit";

    @Before
    public void setUp() throws Exception {
        publishRequest = readStanzaAsIq("/iq/pubsub/publish/iodef.stanza");

        publishIodef = publishRequest.getChildElement().element("publish")
                .element("item").element("IODEF-Document");

        channelManager = Mockito.mock(ChannelManager.class);
    }

    private IodefDocument getIodefDocumentObject(Element payload) {
        IodefDocument validate = new IodefDocument(payload);
        validate.setNode(node);
        validate.setTo(server);
        validate.setUser(jid);
        validate.setChannelManager(channelManager);
        return validate;
    }

    @Test
    public void shouldValidateWellFormedIodefDocument() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertTrue("Well formed IODEF document should be valid",
                validator.isValid());
        Assert.assertTrue("Error message should be empty for valid documents",
                validator.getErrorMessage().isEmpty());
    }

    @Test
    public void notProvidingAnIodefDocumentReturnsError() throws Exception {
        validator = new IodefDocument(null);
        Assert.assertFalse(validator.isValid());
        Assert.assertEquals("iodef-document-element-required",
                validator.getErrorMessage());
    }

    @Test
    public void missingVersionAttributeReturnsInvalid() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.attribute("version").detach();
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("Missing version should be invalid",
                validator.isValid());
        Assert.assertEquals("iodef-version-attribute-required",
                validator.getErrorMessage());
    }

    @Test
    public void unsupportedVersionReturnsInvalid() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.attribute("version").setValue("0.00");
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("Unsupported version should be invalid",
                validator.isValid());
        Assert.assertEquals("iodef-version-not-supported",
                validator.getErrorMessage());
    }

    @Test
    public void missingLangAttributeReturnsInvalid() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.attribute("lang").detach();
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("Missing lang should be invalid",
                validator.isValid());
        Assert.assertEquals("iodef-lang-attribute-required",
                validator.getErrorMessage());
    }

    @Test
    public void missingIncidentReturnsInvalid() throws Exception {

        Assert.assertNotNull(publishIodef.element("Incident"));

        Element iodefDocument = (Element) this.publishIodef.clone();
        iodefDocument.element("Incident").detach();
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse(validator.isValid());
        Assert.assertEquals("incident-element-required",
                validator.getErrorMessage());
    }

    @Test
    public void missingIncidentIdReturnsInvalid() throws Exception {

        Assert.assertNotNull(publishIodef.element("Incident").element(
                "IncidentID"));

        Element iodefDocument = (Element) this.publishIodef.clone();
        iodefDocument.element("Incident").element("IncidentID").detach();
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse(validator.isValid());
        Assert.assertEquals("incident-id-element-required",
                validator.getErrorMessage());
    }

    @Test
    public void emptyIncidentIdReturnsInvalid() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("IncidentID").setText("");
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("Empty IncidentID should be invalid",
                validator.isValid());
        Assert.assertEquals("incident-id-element-required",
                validator.getErrorMessage());
    }

    @Test
    public void missingIncidentIdNameAttributeReturnsInvalid() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("IncidentID")
                .attribute("name").detach();
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("Missing name should be invalid",
                validator.isValid());
        Assert.assertEquals("incident-id-name-attribute-required",
                validator.getErrorMessage());
    }

    @Test
    public void emptyIncidentIdNameAttributeReturnsInvalid() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("IncidentID")
                .attribute("name").setValue("");
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("Empty name should be invalid", validator.isValid());
        Assert.assertEquals("incident-id-name-attribute-required",
                validator.getErrorMessage());
    }

    @Test
    public void missingReportTimeIsAdded() throws Exception {

        Assert.assertNotNull(publishIodef.element("Incident").element(
                "ReportTime"));

        Element iodefDocument = (Element) this.publishIodef.clone();
        iodefDocument.element("Incident").element("ReportTime").detach();
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertTrue("Missing ReportTime should be added",
                validator.isValid());
        iodefDocument = validator.getPayload();
        Assert.assertTrue(iodefDocument
                .element("Incident")
                .elementText("ReportTime")
                .matches(
                        "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3}Z"));
    }

    @Test
    public void emptyReportTimeAdded() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("ReportTime").setText("");
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertTrue("Missing ReportTime should be added",
                validator.isValid());
        iodefDocument = validator.getPayload();
        Assert.assertTrue(iodefDocument
                .element("Incident")
                .elementText("ReportTime")
                .matches(
                        "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3}Z"));
    }

    @Test
    public void missingAssessmentReturnsInvalid() throws Exception {

        Assert.assertNotNull(publishIodef.element("Incident").element(
                "Assessment"));

        Element iodefDocument = (Element) this.publishIodef.clone();
        iodefDocument.element("Incident").element("Assessment").detach();
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse(validator.isValid());
        Assert.assertEquals("assessment-element-required",
                validator.getErrorMessage());
    }

    @Test
    public void missingAnyTypeOfImpactElementReturnsInvalid() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("Assessment")
                .element("Impact").detach();
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("Missing Impact element class should be invalid",
                validator.isValid());
        Assert.assertEquals("impact-element-required",
                validator.getErrorMessage());
    }

    @Test
    public void supportsTimeImpactElementAsImpactType() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("Assessment")
                .element("Impact").setName("TimeImpact");
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertTrue("TimeImpact should be a valid 'Impact' type",
                validator.isValid());
        Assert.assertTrue("Error message should be empty", validator
                .getErrorMessage().isEmpty());
    }

    @Test
    public void supportsMonetaryImpactElementAsImpactType() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("Assessment")
                .element("Impact").setName("MonetaryImpact");
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertTrue("MonetaryImpact should be a valid 'Impact' type",
                validator.isValid());
        Assert.assertTrue("Error message should be empty", validator
                .getErrorMessage().isEmpty());
    }

    @Test
    public void missingImpactLangAttributeReturnsInvalid() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("Assessment")
                .element("Impact").attribute("lang").detach();
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("Missing lang should be invalid",
                validator.isValid());
        Assert.assertEquals("impact-lang-attribute-required",
                validator.getErrorMessage());
    }

    @Test
    public void emptyImpactLangAttributeReturnsInvalid() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("Assessment")
                .element("Impact").attribute("lang").setValue("");
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("Empty lang should be invalid", validator.isValid());
        Assert.assertEquals("impact-lang-attribute-required",
                validator.getErrorMessage());
    }

    @Test
    public void missingImpactTypeAttributeReturnsInvalid() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("Assessment")
                .element("Impact").attribute("type").detach();
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("Missing type should be invalid",
                validator.isValid());
        Assert.assertEquals("impact-type-attribute-required",
                validator.getErrorMessage());
    }

    @Test
    public void emptyImpactTypeAttributeReturnsInvalid() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("Assessment")
                .element("Impact").attribute("type").setValue("");
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("Empty type should be invalid", validator.isValid());
        Assert.assertEquals("impact-type-attribute-required",
                validator.getErrorMessage());
    }

    @Test
    public void missingContactReturnsInvalid() throws Exception {

        Assert.assertNotNull(publishIodef.element("Incident")
                .element("Contact"));

        Element iodefDocument = (Element) this.publishIodef.clone();
        iodefDocument.element("Incident").element("Contact").detach();
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse(validator.isValid());
        Assert.assertEquals("contact-element-required",
                validator.getErrorMessage());
    }

    @Test
    public void missingContactRoleAttributeReturnsInvalid() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("Contact").attribute("role")
                .detach();
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("Missing role should be invalid",
                validator.isValid());
        Assert.assertEquals("contact-role-attribute-required",
                validator.getErrorMessage());
    }

    @Test
    public void emptyContactRoleAttributeReturnsInvalid() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("Contact").attribute("role")
                .setValue("");
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("Empty role should be invalid", validator.isValid());
        Assert.assertEquals("contact-role-attribute-required",
                validator.getErrorMessage());
    }

    @Test
    public void missingContactTypeAttributeReturnsInvalid() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("Contact").attribute("type")
                .detach();
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("Missing type should be invalid",
                validator.isValid());
        Assert.assertEquals("contact-type-attribute-required",
                validator.getErrorMessage());
    }

    @Test
    public void emptyContactTypeAttributeReturnsInvalid() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("Contact").attribute("type")
                .setValue("");
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("Empty type should be invalid", validator.isValid());
        Assert.assertEquals("contact-type-attribute-required",
                validator.getErrorMessage());
    }

    @Test
    public void contactElementWithNoChildReturnsInvalid() throws Exception {

        Assert.assertNotNull(publishIodef.element("Incident")
                .element("Contact"));

        Element iodefDocument = (Element) this.publishIodef.clone();
        iodefDocument.element("Incident").element("Contact")
                .element("ContactName").detach();
        iodefDocument.element("Incident").element("Contact").element("Email")
                .detach();
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("", validator.isValid());
        Assert.assertEquals("contact-child-element-required",
                validator.getErrorMessage());
    }

    @Test
    public void emptyEmailReturnsInvalid() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("Contact").element("Email")
                .setText("");
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertFalse("Empty Email should be invalid", validator.isValid());
        Assert.assertEquals("contact-email-element-must-not-be-empty",
                validator.getErrorMessage());
    }

    @Test
    public void doesNotRequireEmailElement() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        iodefDocument.element("Incident").element("Contact").element("Email")
                .detach();
        validator = getIodefDocumentObject(iodefDocument);
        Assert.assertTrue("Email element should not be mandatory",
                validator.isValid());
        Assert.assertTrue("Error message should be empty", validator
                .getErrorMessage().isEmpty());
    }

    @Test
    public void dealsWithInReplyToGracefully() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        validator = getIodefDocumentObject(iodefDocument);

        Assert.assertNull(validator.getInReplyTo());
    }

    @Test
    public void suppliesGlobalItemId() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        validator = getIodefDocumentObject(iodefDocument);

        String globalItemId = validator.getGlobalItemId();

        Assert.assertNotNull(globalItemId);
        Assert.assertTrue(GlobalItemIDImpl.isGlobalId(globalItemId));
    }

    @Test
    public void requiresIncidentForGlobalItemId() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        validator = getIodefDocumentObject(iodefDocument);

        iodefDocument.element("Incident").detach();
        String globalItemId = validator.getGlobalItemId();

        Assert.assertNull(globalItemId);
    }

    @Test
    public void requiresIncidentIdForGlobalItemId() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();
        Element incidentId = iodefDocument.element("Incident")
                .element("IncidentID");

        validator = getIodefDocumentObject(iodefDocument);

        incidentId.detach();
        String globalItemId = validator.getGlobalItemId();

        Assert.assertNull(globalItemId);
    }

    @Test
    public void requiresIncidentIdNameForGlobalItemId() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();
        Element incidentId = iodefDocument.element("Incident")
                .element("IncidentID");

        validator = getIodefDocumentObject(iodefDocument);

        incidentId.attribute("name").detach();
        String globalItemId = validator.getGlobalItemId();

        Assert.assertNull(globalItemId);
    }

    @Test
    public void suppliesLocalItemId() throws Exception {
        Element iodefDocument = (Element) this.publishIodef.clone();

        validator = getIodefDocumentObject(iodefDocument);

        Element incidentId = iodefDocument.element("Incident")
                                          .element("IncidentID");

        String expectedOriginator = incidentId.attribute("name").getValue();
        String expectedIncidentId = incidentId.getTextTrim();

        String localItemId = validator.getLocalItemId();
        Assert.assertNotNull(localItemId);

        String[] parts = localItemId.split("\\|");
        Assert.assertEquals(2, parts.length);
        Assert.assertEquals(expectedOriginator, parts[0]);
        Assert.assertEquals(expectedIncidentId, parts[1]);
    }
}