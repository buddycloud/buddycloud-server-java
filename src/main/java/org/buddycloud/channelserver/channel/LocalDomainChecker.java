package org.buddycloud.channelserver.channel;

import java.io.IOException;
import java.util.Properties;

import org.buddycloud.channelserver.Configuration;

public class LocalDomainChecker {

	private static final int IS_LOCAL_EXIT_VALUE = 0;
	
	public static boolean isLocal(String domain, Properties configuration) {
		if (configuration == null) {
			return false;
		}
		String command = configuration.getProperty(
				Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER);
		if (command == null) {
			return false;
		}
		int exitValue;
		try {
			exitValue = runChecker(command, domain);
		} catch (Exception e) {
			return false;
		}
		return exitValue == IS_LOCAL_EXIT_VALUE;
	}

	private static int runChecker(String command, String domain) throws IOException,
			InterruptedException {
		if (command.equals(Boolean.TRUE.toString())) {
			return IS_LOCAL_EXIT_VALUE;
		}
		ProcessBuilder processBuilder = new ProcessBuilder(command, domain);
		Process process = processBuilder.start();
		return process.waitFor();
	}
}
