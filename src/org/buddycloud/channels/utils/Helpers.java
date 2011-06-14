package org.buddycloud.channels.utils;

import java.util.AbstractCollection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.StringTokenizer;

public class Helpers {
	/**
     * Returns a comma-delimitted list of Strings as a Collection.
     *
     * @return a Collection representing the String.
     */
    public static Collection<String> stringToCollection(String string) {
        if (string == null || string.trim().length() == 0) {
            return Collections.emptyList();
        }
        Collection<String> collection = new ArrayList<String>();
        StringTokenizer tokens = new StringTokenizer(string, ",");
        while (tokens.hasMoreTokens()) {
            collection.add(tokens.nextToken().trim());
        }
        return collection;
    }
    
    public static String collectionToString(Collection<String> s) {
    	String delimiter = ",";
        if (s.isEmpty()) return "";
        Iterator<String> iter = s.iterator();
        StringBuffer buffer = new StringBuffer(iter.next());
        while (iter.hasNext()) {
            buffer.append(delimiter);
            buffer.append(iter.next());
        }
        return buffer.toString();
    }
}
