package org.buddycloud.channelserver.channel;

import java.util.Date;
import java.util.HashMap;
import org.buddycloud.channelserver.Configuration;

import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.ISODateTimeFormat;
import org.xmpp.packet.JID;

/*
 * Most of these are copied from here
 * https://github.com/buddycloud/buddycloud-server/blob/master/src/local/operations.coffee#L14
 */
public class Conf {

	public static final String TYPE = "pubsub#type";
	public static final String TITLE = "pubsub#title";
	public static final String DESCRIPTION = "pubsub#description";
	public static final String PUBLISH_MODEL = "pubsub#publish_model";
	public static final String ACCESS_MODEL = "pubsub#access_model";
	public static final String CREATION_DATE = "pubsub#creation_date";
	public static final String OWNER = "pubsub#owner";
	public static final String DEFAULT_AFFILIATION = "buddycloud#default_affiliation";
	public static final String NUM_SUBSCRIBERS = "pubsub#num_subscribers";
	public static final String NOTIFY_CONFIG = "pubsub#notify_config";
	public static final String CHANNEL_TYPE = "buddycloud#channel_type";
	private static final String PUBLISHERS = "publishers";
	public static final DateTimeFormatter ISO_8601_PARSER = ISODateTimeFormat.dateTimeParser();
	public static final DateTimeFormatter ISO_8601_FORMATTER = ISODateTimeFormat.dateTime();

	public static String getPostChannelNodename(JID channelJID) {
		return "/user/" + channelJID.toBareJID() + "/posts";
	}

	/**
	 * Parses a ISO 8601 to a string
	 *
	 * @param iso8601Str
	 * @return
	 * @throws IllegalArgumentException if the provided string is not ISO 8601
	 */
	public static Date parseDate(String iso8601Str) throws IllegalArgumentException {
		return ISO_8601_PARSER.parseDateTime(iso8601Str).toDate();
	}

	public static String formatDate(Date date) {
		return ISO_8601_FORMATTER.print(date.getTime());
	}

	public static HashMap<String, String> getDefaultChannelConf(JID channelJID, JID ownerJID) {
		HashMap<String, String> conf = getDefaultConf(channelJID, null);
		conf.put(TITLE, channelJID.toBareJID() + "'s title");
		conf.put(DESCRIPTION, channelJID.toBareJID() + "'s description");
		conf.put(OWNER, ownerJID.toBareJID());
		return conf;
	}

	public static HashMap<String, String> getDefaultPostChannelConf(JID channelJID) {
		HashMap<String, String> conf = getDefaultConf(channelJID, "posts");
		conf.put(TITLE, channelJID.toBareJID() + "'s very own buddycloud channel!");
		conf.put(DESCRIPTION, "This channel belongs to " + channelJID.toBareJID() + ". To nobody else!");
		conf.put(CHANNEL_TYPE, "personal");
		return conf;
	}

	public static String getStatusChannelNodename(JID channelJID) {
		return "/user/" + channelJID.toBareJID() + "/status";
	}

	public static HashMap<String, String> getDefaultStatusChannelConf(JID channelJID) {
		HashMap<String, String> conf = getDefaultConf(channelJID, "status");
		conf.put(TITLE, channelJID.toBareJID() + "'s very own buddycloud status!");
		conf.put(DESCRIPTION, "This is " + channelJID.toBareJID() + "'s mood a.k.a status -channel. Depends how geek you are.");
		return conf;
	}

	public static String getGeoPreviousChannelNodename(JID channelJID) {
		return "/user/" + channelJID.toBareJID() + "/geo/previous";
	}

	public static HashMap<String, String> getDefaultGeoPreviousChannelConf(JID channelJID) {
		HashMap<String, String> conf = getDefaultConf(channelJID, "geo/previous");
		conf.put(TITLE, channelJID.toBareJID() + "'s previous location.");
		conf.put(DESCRIPTION, "Where " + channelJID.toBareJID() + " has been before.");
		return conf;
	}

	public static String getGeoCurrentChannelNodename(JID channelJID) {
		return "/user/" + channelJID.toBareJID() + "/geo/current";
	}

	public static HashMap<String, String> getDefaultGeoCurrentChannelConf(JID channelJID) {
		HashMap<String, String> conf = getDefaultConf(channelJID, "geo/current");
		conf.put(TITLE, channelJID.toBareJID() + "'s current location.");
		conf.put(DESCRIPTION, "Where " + channelJID.toBareJID() + " is now.");
		return conf;
	}

	public static String getGeoNextChannelNodename(JID channelJID) {
		return "/user/" + channelJID.toBareJID() + "/geo/next";
	}

	public static HashMap<String, String> getDefaultGeoNextChannelConf(JID channelJID) {
		HashMap<String, String> conf = getDefaultConf(channelJID, "geo/next");
		conf.put(TITLE, channelJID.toBareJID() + "'s next location.");
		conf.put(DESCRIPTION, "Where " + channelJID.toBareJID() + " is going to go.");
		return conf;
	}

	public static String getSubscriptionsChannelNodename(JID channelJID) {
		return "/user/" + channelJID.toBareJID() + "/subscriptions";
	}

	public static HashMap<String, String> getDefaultSubscriptionsChannelConf(JID channelJID) {
		HashMap<String, String> conf = getDefaultConf(channelJID, "subscriptions");
		conf.put(TITLE, channelJID.toBareJID() + "'s susbcriptions.");
		conf.put(DESCRIPTION, channelJID.toBareJID() + "'s subscriptions. ");
		return conf;
	}

	private static HashMap<String, String> getDefaultConf(JID channelJID, String node) {
		
		HashMap<String, String> conf = new HashMap<String, String>();
		Configuration projectConf = Configuration.getInstance();
		conf.put(TYPE, "http://www.w3.org/2005/Atom");
		conf.put(PUBLISH_MODEL, PUBLISHERS);
		conf.put(CREATION_DATE, formatDate(new Date()));
		conf.put(OWNER, channelJID.toBareJID());
		conf.put(ACCESS_MODEL, AccessModels.createFromString(projectConf.getProperty(
				Configuration.CONFIGURATION_CHANNELS_DEFAULT_ACCESSMODEL, AccessModels.authorize.toString())).toString());
		conf.put(DEFAULT_AFFILIATION, Affiliations.createFromString(
				projectConf.getProperty(Configuration.CONFIGURATION_CHANNELS_DEFAULT_AFFILIATION, Affiliations.member.toString()))
				.toString());
		conf.put(NUM_SUBSCRIBERS, "1");
		conf.put(NOTIFY_CONFIG, "1");
		
		if (null != node) {
			String accessModelKey = Configuration.CONFIGURATION_CHANNELS_DEFAULT_ACCESSMODEL.replace("default", node.replace("/", "."));
			if (null != projectConf.getProperty(accessModelKey)) {
			    conf.put(ACCESS_MODEL, AccessModels.createFromString(projectConf.getProperty(
					accessModelKey)).toString());
			}
			
			String affiliationKey = Configuration.CONFIGURATION_CHANNELS_DEFAULT_AFFILIATION.replace("default", node.replace("/", "."));
			if (null != projectConf.getProperty(affiliationKey)) {
			    conf.put(DEFAULT_AFFILIATION, Affiliations.createFromString(projectConf.getProperty(
			    		affiliationKey)).toString());
			}
		}
		return conf;
	}
}
