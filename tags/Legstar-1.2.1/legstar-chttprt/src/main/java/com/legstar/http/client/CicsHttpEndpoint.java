/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.http.client;

import org.apache.commons.configuration.HierarchicalConfiguration;

/**
 * This class represents the parameters that are necessary for a client
 * to sucessfully connect to CICS over Http.
 */
public class CicsHttpEndpoint {
	
	/** Host IP address. */
	private String mHostIPAddress;

	/** Host IP port. */
	private int mHostIPPort;

	/** Host charset. */
	private String mHostCharset;

	/** Host user ID. */
	private String mHostUserID;

	/** Host password. */
	private String mHostPassword;

	/** Host trace mode. */
	private boolean mHostTraceMode;

	/** Path to the HTTP server on the Host. */
	private String mHostURLPath;
	
	/** Default URL path to the CICS Http server program. */
	private static final String DEFAULT_HOST_URL_PATH = "/CICS/CWBA/LSWEBBIN";
	
	/** Configuration XPath location for CICS HTTP Path. */
	private static final String HOST_URL_PATH_CFG = "hostURLPath";
	
	/** Configuration XPath location for IP address. */
	private static final String IP_ADDRESS_CFG = "hostIPAddress";

	/** Configuration XPath location for IP port. */
	private static final String IP_PORT_CFG = "hostIPPort";

	/** Configuration XPath location for host charset. */
	private static final String HOST_CHARSET_CFG = "hostCharset";

	/** Configuration XPath location for host user ID. */
	private static final String HOST_USERID_CFG = "hostUserID";
	
	/** Configuration XPath location for host password. */
	private static final String HOST_PASSWORD_CFG = "hostPassword";
	
	/** Configuration XPath location for host trace mode. */
	private static final String HOST_TRACE_CFG = "hostTraceMode";
	
	/** Query parm on URL tells the host to enter trace mode. */
	private static final String HOST_TRACE_QRY = "?trace";

	
	/**
	 * No-argument constructor.
	 */
	public CicsHttpEndpoint() {
		
	}
	
	/**
	 * Constructor from a configuration fragment.
	 * @param config a configuration sub hierarchy
	 */
	public CicsHttpEndpoint(final HierarchicalConfiguration config) {
		
		/* Get default connection parameters from the configuration */
		mHostIPAddress = config.getString(IP_ADDRESS_CFG);
		mHostIPPort = config.getInt(IP_PORT_CFG, 0);
		mHostCharset = config.getString(HOST_CHARSET_CFG);
		mHostUserID = config.getString(HOST_USERID_CFG);
		mHostPassword = config.getString(HOST_PASSWORD_CFG);
		mHostTraceMode = config.getBoolean(HOST_TRACE_CFG, false);
		mHostURLPath = config.getString(HOST_URL_PATH_CFG,
				DEFAULT_HOST_URL_PATH);
	}
	
	/**
	 * Helper to pretty print the endpoint content.
	 * @return formatted endpoint report
	 */
	public final String getReport() {
		String report = "CICS Http endpoint:"
			+ "  " + IP_ADDRESS_CFG + "=" + mHostIPAddress + ","
			+ "  " + IP_PORT_CFG + "=" + mHostIPPort + ","
			+ "  " + HOST_URL_PATH_CFG + "=" + mHostURLPath + ","
			+ "  " + HOST_CHARSET_CFG + "=" + mHostCharset + ","
			+ "  " + HOST_USERID_CFG + "=" + mHostUserID + ","
			+ "  " + HOST_TRACE_CFG + "=" + mHostTraceMode;
		return report;
	}
	
	/**
	 * @return the host charset
	 */
	public final String getHostCharset() {
		return mHostCharset;
	}

	/**
	 * @param hostCharset the host charset to set
	 */
	public final void setHostCharset(final String hostCharset) {
		mHostCharset = hostCharset;
	}

	/**
	 * @return the host IP address
	 */
	public final String getHostIPAddress() {
		return mHostIPAddress;
	}

	/**
	 * @param hostIPAddress the host IP address to set
	 */
	public final void setHostIPAddress(final String hostIPAddress) {
		mHostIPAddress = hostIPAddress;
	}

	/**
	 * @return the host IP port
	 */
	public final int getHostIPPort() {
		return mHostIPPort;
	}

	/**
	 * @param hostIPPort the host IP port to set
	 */
	public final void setHostIPPort(final int hostIPPort) {
		mHostIPPort = hostIPPort;
	}

	/**
	 * @return the host password
	 */
	public final String getHostPassword() {
		return mHostPassword;
	}

	/**
	 * @param hostPassword the host password to set
	 */
	public final void setHostPassword(final String hostPassword) {
		mHostPassword = hostPassword;
	}

	/**
	 * @return the host trace mode enabled or or
	 */
	public final boolean isHostTraceMode() {
		return mHostTraceMode;
	}

	/**
	 * @param hostTraceMode the host trace mode to set
	 */
	public final void setHostTraceMode(final boolean hostTraceMode) {
		mHostTraceMode = hostTraceMode;
	}

	/**
	 * @return the host user ID
	 */
	public final String getHostUserID() {
		return mHostUserID;
	}

	/**
	 * @param hostUserID the host user ID to set
	 */
	public final void setHostUserID(final String hostUserID) {
		mHostUserID = hostUserID;
	}

	/**
	 * This method adds a query parm if host trace mode is on.
	 * @return the path to the CICS Http server program
	 */
	public final String getHostURLPath() {
		if (mHostTraceMode && !mHostURLPath.contains(HOST_TRACE_QRY)) {
			return mHostURLPath + HOST_TRACE_QRY;
		}
		return mHostURLPath;
	}

	/**
	 * @param hostURLPath the path to the CICS Http server program to set
	 */
	public final void setHostURLPath(final String hostURLPath) {
		mHostURLPath = hostURLPath;
	}

}
