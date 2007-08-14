/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.mq.client;

import org.apache.commons.configuration.HierarchicalConfiguration;

/**
 * This class represents the parameters that are necessary for a client
 * to sucessfully connect to CICS over MQ.
 */
public class CicsMQEndpoint {
	
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

	/** Host MQ Manager. */
	private String mHostMQManager;

	/** Host MQ Channel. */
	private String mHostMQChannel;

	/** Host MQ Request queue name. */
	private String mHostMQRequestQueue;

	/** Host MQ Reply queue name. */
	private String mHostMQResponseQueue;

	/** Host trace mode. */
	private boolean mHostTraceMode;

	/** Default MQ Manager. */
	private static final String DEFAULT_MQ_MANAGER = "CSQ1";
	
	/** Default MQ IP port. */
	private static final int DEFAULT_MQ_PORT = 1414;
	
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
	
	/** Configuration XPath location for MQ Manager. */
	private static final String HOST_MQ_MANAGER_CFG = "hostMQManager";
	
	/** Configuration XPath location for MQ Channel. */
	private static final String HOST_MQ_CHANNEL_CFG = "hostMQChannel";
	
	/** Configuration XPath location for MQ Request queue. */
	private static final String HOST_MQ_REQUEST_Q_CFG = "hostMQRequestQueue";
	
	/** Configuration XPath location for MQ Reply queue. */
	private static final String HOST_MQ_RESPONSE_Q_CFG = "hostMQResponseQueue";
	
	
	/**
	 * No-argument constructor.
	 */
	public CicsMQEndpoint() {
		
	}
	
	/**
	 * Constructor from a configuration fragment.
	 * @param config a configuration sub hierarchy
	 */
	public CicsMQEndpoint(final HierarchicalConfiguration config) {
		
		/* Get default connection parameters from the configuration */
		mHostIPAddress = config.getString(IP_ADDRESS_CFG);
		mHostIPPort = config.getInt(IP_PORT_CFG, DEFAULT_MQ_PORT);
		mHostCharset = config.getString(HOST_CHARSET_CFG);
		mHostUserID = config.getString(HOST_USERID_CFG);
		mHostPassword = config.getString(HOST_PASSWORD_CFG);
		mHostMQManager = config.getString(HOST_MQ_MANAGER_CFG,
				DEFAULT_MQ_MANAGER);
		mHostMQChannel = config.getString(HOST_MQ_CHANNEL_CFG);
		mHostMQRequestQueue = config.getString(HOST_MQ_REQUEST_Q_CFG);
		mHostMQResponseQueue = config.getString(HOST_MQ_RESPONSE_Q_CFG);
		mHostTraceMode = config.getBoolean(HOST_TRACE_CFG, false);
	}
	
	/**
	 * Helper to pretty print the endpoint content.
	 * @return formatted endpoint report
	 */
	public final String getReport() {
		String report = "CICS Http endpoint:"
			+ "  " + IP_ADDRESS_CFG + "=" + mHostIPAddress + ","
			+ "  " + IP_PORT_CFG + "=" + mHostIPPort + ","
			+ "  " + HOST_MQ_MANAGER_CFG + "=" + mHostMQManager + ","
			+ "  " + HOST_MQ_CHANNEL_CFG + "=" + mHostMQChannel + ","
			+ "  " + HOST_MQ_REQUEST_Q_CFG + "=" + mHostMQRequestQueue + ","
			+ "  " + HOST_MQ_RESPONSE_Q_CFG + "=" + mHostMQResponseQueue + ","
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
	 * @return the MQ Channel
	 */
	public final String getHostMQChannel() {
		return mHostMQChannel;
	}

	/**
	 * @param hostMQChannel the MQ Channel to set
	 */
	public final void setHostMQChannel(final String hostMQChannel) {
		mHostMQChannel = hostMQChannel;
	}

	/**
	 * @return the MQ Manager
	 */
	public final String getHostMQManager() {
		return mHostMQManager;
	}

	/**
	 * @param hostMQManager the MQ Manager to set
	 */
	public final void setHostMQManager(final String hostMQManager) {
		mHostMQManager = hostMQManager;
	}

	/**
	 * @return the MQ Reply queue
	 */
	public final String getHostMQResponseQueue() {
		return mHostMQResponseQueue;
	}

	/**
	 * @param hostMQReplyQueue the MQ Reply queue to set
	 */
	public final void setHostMQResponseQueue(final String hostMQReplyQueue) {
		mHostMQResponseQueue = hostMQReplyQueue;
	}

	/**
	 * @return the MQ Request queue
	 */
	public final String getHostMQRequestQueue() {
		return mHostMQRequestQueue;
	}

	/**
	 * @param hostMQRequestQueue the MQ Request queue to set
	 */
	public final void setHostMQRequestQueue(final String hostMQRequestQueue) {
		mHostMQRequestQueue = hostMQRequestQueue;
	}

}
