package org.buddycloud.channelserver.channel;

import java.io.InputStream;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

import org.apache.commons.io.IOUtils;
import org.buddycloud.channelserver.Configuration;

public class LocalDomainChecker {

    private static final String COMMA = ",";
    
    public static boolean isLocal(String domain, Properties configuration) {
        return isLocal(domain, configuration, null);
    }
    
    public static boolean isLocal(String domain, Properties configuration, Set<String> localDomains) {
        if (configuration == null) {
            return false;
        }
        String command = configuration.getProperty(
                Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER);
        
        if (command == null) {
            String serverDomain = configuration
                    .getProperty(Configuration.CONFIGURATION_SERVER_DOMAIN);
            String topicDomain = configuration
                    .getProperty(Configuration.CONFIGURATION_SERVER_TOPICS_DOMAIN);
            String channelDomain = configuration
                    .getProperty(Configuration.CONFIGURATION_SERVER_CHANNELS_DOMAIN);
            return (domain.equals(serverDomain) || domain.equals(topicDomain)
                    || domain.equals(channelDomain));
        }
        
        if (command.equals(Boolean.TRUE.toString())) {
            return true;
        }
        
        if (localDomains == null) {
            try {
                localDomains = getLocalDomains(configuration);
            } catch (Exception e) {
                return false;
            }
        }
        return localDomains.contains(domain.toLowerCase());
    }

    public static Set<String> getLocalDomains(Properties configuration) {
        HashSet<String> localDomains = new HashSet<String>();
        String command = configuration.getProperty(
                Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER);
        if (command == null) {
            return localDomains;
        }
        
        try {
            ProcessBuilder processBuilder = new ProcessBuilder(command);
            Process process = processBuilder.start();
            process.waitFor();
            InputStream inputStream = process.getInputStream();
            
            String csvDomains = IOUtils.toString(inputStream);
            for (String eachDomain : csvDomains.split(COMMA)) {
                localDomains.add(eachDomain.trim().toLowerCase());
            }
            inputStream.close();
        } catch (Exception e) {
            // Return empty set
        }
        
        return localDomains;
    }
}
