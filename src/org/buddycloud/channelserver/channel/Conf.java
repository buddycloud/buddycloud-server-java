package org.buddycloud.channelserver.channel;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.TimeZone;

//TODO! Refactor this!
// Lot's of duplicate code plus other mayhem (like the SimpleDateFormat.
public class Conf {

    public static final String TYPE                = "pubsub#type";
    public static final String TITLE               = "pubsub#title";
    public static final String DESCRIPTION         = "pubsub#description";
    public static final String PUBLISH_MODEL       = "pubsub#publish_model";
    public static final String ACCSES_MODEL        = "pubsub#access_model";
    public static final String CREATION_DATE       = "pubsub#creation_date";
    public static final String OWNER               = "pubsub#owner";
    public static final String DEFUALT_AFFILIATION = "pubsub#default_affiliation";
    public static final String NUM_SUBSCRIBERS     = "pubsub#num_subscribers";
    public static final String NOTIFY_CONFIG       = "pubsub#notify_config";
    public static final String CHANNEL_TYPE        = "buddycloud#channel_type";
    
    // Most of these are copied from here
    // https://github.com/buddycloud/buddycloud-server/blob/master/src/local/operations.coffee#L14
    
    public static String getPostChannelNodename(String bareJID) {
        return "/user/" + bareJID + "/posts";
    }

    public static HashMap<String, String> getDefaultPostChannelConf(String bareJID) {
        
        // TODO! Refactor this!
        
        String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        
        HashMap<String, String> conf = new HashMap<String, String>();
        
        conf.put(TYPE, "http://www.w3.org/2005/Atom");
        conf.put(TITLE, bareJID + "'s very own buddycloud channel!");
        conf.put(DESCRIPTION, "This channel belongs to " + bareJID + ". To nobody else!");
        conf.put(PUBLISH_MODEL, org.buddycloud.channelserver.pubsub.publishmodel.PublishModels.subscribers.toString());
        conf.put(ACCSES_MODEL, org.buddycloud.channelserver.pubsub.accessmodel.AccessModels.open.toString());
        conf.put(CREATION_DATE, sdf.format(new Date()));
        conf.put(OWNER, bareJID);
        conf.put(DEFUALT_AFFILIATION, org.buddycloud.channelserver.pubsub.affiliation.Affiliations.member.toString());
        conf.put(NUM_SUBSCRIBERS, "1");
        conf.put(NOTIFY_CONFIG, "1");
        conf.put(CHANNEL_TYPE, "personal");
        
        return conf;
    }
    
    public static String getStatusChannelNodename(String bareJID) {
        return "/user/" + bareJID + "/status";
    }
    
    public static HashMap<String, String> getDefaultStatusChannelConf(String bareJID) {
        
        // TODO! Refactor this!
        
        String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        
        HashMap<String, String> conf = new HashMap<String, String>();
        
        conf.put(TYPE, "http://www.w3.org/2005/Atom");
        conf.put(TITLE, bareJID + "'s very own buddycloud status!");
        conf.put(DESCRIPTION, "This is " + bareJID + "'s mood a.k.a status -channel. Depends how geek you are.");
        conf.put(PUBLISH_MODEL, org.buddycloud.channelserver.pubsub.publishmodel.PublishModels.publishers.toString());
        conf.put(ACCSES_MODEL, org.buddycloud.channelserver.pubsub.accessmodel.AccessModels.open.toString());
        conf.put(CREATION_DATE, sdf.format(new Date()));
        conf.put(OWNER, bareJID);
        conf.put(DEFUALT_AFFILIATION, org.buddycloud.channelserver.pubsub.affiliation.Affiliations.member.toString());
        conf.put(NUM_SUBSCRIBERS, "1");
        conf.put(NOTIFY_CONFIG, "1");
        
        return conf;
    }
    
    public static String getGeoPreviousChannelNodename(String bareJID) {
        return "/user/" + bareJID + "/geo/previous";
    }
    
    public static HashMap<String, String> getDefaultGeoPreviousChannelConf(String bareJID) {
        
        // TODO! Refactor this!
        
        String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        
        HashMap<String, String> conf = new HashMap<String, String>();
        
        conf.put(TYPE, "http://www.w3.org/2005/Atom");
        conf.put(TITLE, bareJID + "'s previous location.");
        conf.put(DESCRIPTION, "Where " + bareJID + " has been before.");
        conf.put(PUBLISH_MODEL, org.buddycloud.channelserver.pubsub.publishmodel.PublishModels.publishers.toString());
        conf.put(ACCSES_MODEL, org.buddycloud.channelserver.pubsub.accessmodel.AccessModels.open.toString());
        conf.put(CREATION_DATE, sdf.format(new Date()));
        conf.put(OWNER, bareJID);
        conf.put(DEFUALT_AFFILIATION, org.buddycloud.channelserver.pubsub.affiliation.Affiliations.member.toString());
        conf.put(NUM_SUBSCRIBERS, "1");
        conf.put(NOTIFY_CONFIG, "1");
        
        return conf;
    }
    
    public static String getGeoCurrentChannelNodename(String bareJID) {
        return "/user/" + bareJID + "/geo/current";
    }
    
    public static HashMap<String, String> getDefaultGeoCurrentChannelConf(String bareJID) {
        
        // TODO! Refactor this!
        
        String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        
        HashMap<String, String> conf = new HashMap<String, String>();
        
        conf.put(TYPE, "http://www.w3.org/2005/Atom");
        conf.put(TITLE, bareJID + "'s current location.");
        conf.put(DESCRIPTION, "Where " + bareJID + " is now.");
        conf.put(PUBLISH_MODEL, org.buddycloud.channelserver.pubsub.publishmodel.PublishModels.publishers.toString());
        conf.put(ACCSES_MODEL, org.buddycloud.channelserver.pubsub.accessmodel.AccessModels.open.toString());
        conf.put(CREATION_DATE, sdf.format(new Date()));
        conf.put(OWNER, bareJID);
        conf.put(DEFUALT_AFFILIATION, org.buddycloud.channelserver.pubsub.affiliation.Affiliations.member.toString());
        conf.put(NUM_SUBSCRIBERS, "1");
        conf.put(NOTIFY_CONFIG, "1");
        
        return conf;
    }
    
    public static String getGeoNextChannelNodename(String bareJID) {
        return "/user/" + bareJID + "/geo/next";
    }
    
    public static HashMap<String, String> getDefaultGeoNextChannelConf(String bareJID) {
        
        // TODO! Refactor this!
        
        String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        
        HashMap<String, String> conf = new HashMap<String, String>();
        
        conf.put(TYPE, "http://www.w3.org/2005/Atom");
        conf.put(TITLE, bareJID + "'s next location.");
        conf.put(DESCRIPTION, "Where " + bareJID + " is going to go.");
        conf.put(PUBLISH_MODEL, org.buddycloud.channelserver.pubsub.publishmodel.PublishModels.publishers.toString());
        conf.put(ACCSES_MODEL, org.buddycloud.channelserver.pubsub.accessmodel.AccessModels.open.toString());
        conf.put(CREATION_DATE, sdf.format(new Date()));
        conf.put(OWNER, bareJID);
        conf.put(DEFUALT_AFFILIATION, org.buddycloud.channelserver.pubsub.affiliation.Affiliations.member.toString());
        conf.put(NUM_SUBSCRIBERS, "1");
        conf.put(NOTIFY_CONFIG, "1");
        
        return conf;
    }
    
    public static String getSubscriptionsChannelNodename(String bareJID) {
        return "/user/" + bareJID + "/subscriptions";
    }
    
    public static HashMap<String, String> getDefaultSubscriptionsChannelConf(String bareJID) {
        
        // TODO! Refactor this!
        
        String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        
        HashMap<String, String> conf = new HashMap<String, String>();
        
        conf.put(TYPE, "http://www.w3.org/2005/Atom");
        conf.put(TITLE, bareJID + "'s susbcriptions.");
        conf.put(DESCRIPTION, bareJID + "'s subscriptions. ");
        conf.put(PUBLISH_MODEL, org.buddycloud.channelserver.pubsub.publishmodel.PublishModels.publishers.toString());
        conf.put(ACCSES_MODEL, org.buddycloud.channelserver.pubsub.accessmodel.AccessModels.open.toString());
        conf.put(CREATION_DATE, sdf.format(new Date()));
        conf.put(OWNER, bareJID);
        conf.put(DEFUALT_AFFILIATION, org.buddycloud.channelserver.pubsub.affiliation.Affiliations.member.toString());
        conf.put(NUM_SUBSCRIBERS, "1");
        conf.put(NOTIFY_CONFIG, "1");
        
        return conf;
    }
}
