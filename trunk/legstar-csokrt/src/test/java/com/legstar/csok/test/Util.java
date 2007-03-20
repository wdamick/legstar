package com.legstar.csok.test;

import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import com.legstar.csok.client.CicsSocketEndpoint;

public class Util {
	/** Configuration XPath location for an endpoint. */
	private static final String HOST_ENDPOINT_CFG = "hostEndPoints/hostEndPoint";
	
	public static final String CONFIG_FILE = "config.xml";
	/** Utility method to pretty pring a byte array content. */
	public static String toHexString(final byte[] hostBytes) {
		
		if (hostBytes == null) {
			return null;
		}
		
		StringBuffer hexString = new StringBuffer("");
		for (int i = 0; i < hostBytes.length; i++) {
			hexString.append(
					Integer.toHexString(
							hostBytes[i] & 0xFF | 0x100).substring(1, 3));
		}
		
		return hexString.toString();
	}
	
	public static byte[] toByteArray(final String string) {
		if (string == null) {
			return null;
		}
		byte[] hostBytes = new byte[string.length() / 2];
		for (int i = 0; i < string.length(); i += 2) {
			hostBytes[i / 2] = 
				(byte) Integer.parseInt(string.substring(i, i + 2), 16);
		}
		return hostBytes;
	}

	public static CicsSocketEndpoint getEndpoint(String name) throws ConfigurationException {
		XMLConfiguration config = new XMLConfiguration(CONFIG_FILE);
		config.setExpressionEngine(new XPathExpressionEngine());
		String strXPath = HOST_ENDPOINT_CFG	+ "[@name='" + name + "']";
		List  endpoints = config.configurationsAt(strXPath);
		if (endpoints == null || endpoints.isEmpty()) {
			throw new RuntimeException("The requested endpoint:" 
					+ name
					+ " is not defined in the " + config.getBasePath() + " file.");
		}
		HierarchicalConfiguration sub = (HierarchicalConfiguration) endpoints.get(0);
		CicsSocketEndpoint cicsSocketEndpoint = new CicsSocketEndpoint(sub);
		return cicsSocketEndpoint;

	}

}
