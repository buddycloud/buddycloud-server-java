package org.buddycloud.channelserver.channel;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.TimeZone;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.publishmodel.PublishModels;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.xmpp.packet.JID;

//TODO! Refactor this!
// Lot's of duplicate code plus other mayhem (like the SimpleDateFormat.
public class Conf {

    public static final String TYPE                = "pubsub#type";
    public static final String TITLE               = "pubsub#title";
    public static final String DESCRIPTION         = "pubsub#description";
    public static final String PUBLISH_MODEL       = "pubsub#publish_model";
    public static final String ACCESS_MODEL        = "pubsub#access_model";
    public static final String CREATION_DATE       = "pubsub#creation_date";
    public static final String OWNER               = "pubsub#owner";
    public static final String DEFAULT_AFFILIATION = "buddycloud#default_affiliation";
    public static final String NUM_SUBSCRIBERS     = "pubsub#num_subscribers";
    public static final String NOTIFY_CONFIG       = "pubsub#notify_config";
    public static final String CHANNEL_TYPE        = "buddycloud#channel_type";
    
	private static final String PUBLISHERS = "publishers";
    
    // Most of these are copied from here
    // https://github.com/buddycloud/buddycloud-server/blob/master/src/local/operations.coffee#L14
    
    public static String getPostChannelNodename(JID channelJID) {
        return "/user/" + channelJID.toBareJID() + "/posts";
    }

    public static HashMap<String, String> getDefaultPostChannelConf(JID channelJID) {
        
        // TODO! Refactor this!
        
        String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        
        HashMap<String, String> conf = new HashMap<String, String>();
        
        conf.put(TYPE, "http://www.w3.org/2005/Atom");
        conf.put(TITLE, channelJID.toBareJID() + "'s very own buddycloud channel!");
        conf.put(DESCRIPTION, "This channel belongs to " + channelJID.toBareJID() + ". To nobody else!");
        conf.put(PUBLISH_MODEL, PUBLISHERS);
        conf.put(ACCESS_MODEL, AccessModels.open.toString());
        conf.put(CREATION_DATE, sdf.format(new Date()));
        conf.put(OWNER, channelJID.toBareJID());
        conf.put(DEFAULT_AFFILIATION, Affiliations.member.toString());
        conf.put(NUM_SUBSCRIBERS, "1");
        conf.put(NOTIFY_CONFIG, "1");
        conf.put(CHANNEL_TYPE, "personal");
        
        return conf;
    }
    
    public static String getStatusChannelNodename(JID channelJID) {
        return "/user/" + channelJID.toBareJID() + "/status";
    }
    
    public static HashMap<String, String> getDefaultStatusChannelConf(JID channelJID) {
        
        // TODO! Refactor this!
        
        String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        
        HashMap<String, String> conf = new HashMap<String, String>();
        
        conf.put(TYPE, "http://www.w3.org/2005/Atom");
        conf.put(TITLE, channelJID.toBareJID() + "'s very own buddycloud status!");
        conf.put(DESCRIPTION, "This is " + channelJID.toBareJID() + "'s mood a.k.a status -channel. Depends how geek you are.");
        conf.put(PUBLISH_MODEL, PUBLISHERS);
        conf.put(ACCESS_MODEL, AccessModels.open.toString());
        conf.put(CREATION_DATE, sdf.format(new Date()));
        conf.put(OWNER, channelJID.toBareJID());
        conf.put(DEFAULT_AFFILIATION, Affiliations.member.toString());
        conf.put(NUM_SUBSCRIBERS, "1");
        conf.put(NOTIFY_CONFIG, "1");
        
        return conf;
    }
    
    public static String getGeoPreviousChannelNodename(JID channelJID) {
        return "/user/" + channelJID.toBareJID() + "/geo/previous";
    }
    
    public static HashMap<String, String> getDefaultGeoPreviousChannelConf(JID channelJID) {
        
        // TODO! Refactor this!
        
        String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        
        HashMap<String, String> conf = new HashMap<String, String>();
        
        conf.put(TYPE, "http://www.w3.org/2005/Atom");
        conf.put(TITLE, channelJID.toBareJID() + "'s previous location.");
        conf.put(DESCRIPTION, "Where " + channelJID.toBareJID() + " has been before.");
        conf.put(PUBLISH_MODEL, PUBLISHERS);
        conf.put(ACCESS_MODEL, AccessModels.open.toString());
        conf.put(CREATION_DATE, sdf.format(new Date()));
        conf.put(OWNER, channelJID.toBareJID());
        conf.put(DEFAULT_AFFILIATION, Affiliations.member.toString());
        conf.put(NUM_SUBSCRIBERS, "1");
        conf.put(NOTIFY_CONFIG, "1");
        
        return conf;
    }
    
    public static String getGeoCurrentChannelNodename(JID channelJID) {
        return "/user/" + channelJID.toBareJID() + "/geo/current";
    }
    
    public static HashMap<String, String> getDefaultGeoCurrentChannelConf(JID channelJID) {
        
        // TODO! Refactor this!
        
        String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        
        HashMap<String, String> conf = new HashMap<String, String>();
        
        conf.put(TYPE, "http://www.w3.org/2005/Atom");
        conf.put(TITLE, channelJID.toBareJID() + "'s current location.");
        conf.put(DESCRIPTION, "Where " + channelJID.toBareJID() + " is now.");
        conf.put(PUBLISH_MODEL, PUBLISHERS);
        conf.put(ACCESS_MODEL, AccessModels.open.toString());
        conf.put(CREATION_DATE, sdf.format(new Date()));
        conf.put(OWNER, channelJID.toBareJID());
        conf.put(DEFAULT_AFFILIATION, Affiliations.member.toString());
        conf.put(NUM_SUBSCRIBERS, "1");
        conf.put(NOTIFY_CONFIG, "1");
        
        return conf;
    }
    
    public static String getGeoNextChannelNodename(JID channelJID) {
        return "/user/" + channelJID.toBareJID() + "/geo/next";
    }
    
    public static HashMap<String, String> getDefaultGeoNextChannelConf(JID channelJID) {
        
        // TODO! Refactor this!
        
        String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        
        HashMap<String, String> conf = new HashMap<String, String>();
        
        conf.put(TYPE, "http://www.w3.org/2005/Atom");
        conf.put(TITLE, channelJID.toBareJID() + "'s next location.");
        conf.put(DESCRIPTION, "Where " + channelJID.toBareJID() + " is going to go.");
        conf.put(PUBLISH_MODEL, PUBLISHERS);
        conf.put(ACCESS_MODEL, AccessModels.open.toString());
        conf.put(CREATION_DATE, sdf.format(new Date()));
        conf.put(OWNER, channelJID.toBareJID());
        conf.put(DEFAULT_AFFILIATION, Affiliations.member.toString());
        conf.put(NUM_SUBSCRIBERS, "1");
        conf.put(NOTIFY_CONFIG, "1");
        
        return conf;
    }
    
    public static String getSubscriptionsChannelNodename(JID channelJID) {
        return "/user/" + channelJID.toBareJID() + "/subscriptions";
    }
    
    public static HashMap<String, String> getDefaultSubscriptionsChannelConf(JID channelJID) {
        
        // TODO! Refactor this!
        
        String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        
        HashMap<String, String> conf = new HashMap<String, String>();
        
        conf.put(TYPE, "http://www.w3.org/2005/Atom");
        conf.put(TITLE, channelJID.toBareJID() + "'s susbcriptions.");
        conf.put(DESCRIPTION, channelJID.toBareJID() + "'s subscriptions. ");
        conf.put(PUBLISH_MODEL, PUBLISHERS);
        conf.put(ACCESS_MODEL, AccessModels.open.toString());
        conf.put(CREATION_DATE, sdf.format(new Date()));
        conf.put(OWNER, channelJID.toBareJID());
        conf.put(DEFAULT_AFFILIATION, Affiliations.member.toString());
        conf.put(NUM_SUBSCRIBERS, "1");
        conf.put(NOTIFY_CONFIG, "1");
        
        return conf;
    }
}
