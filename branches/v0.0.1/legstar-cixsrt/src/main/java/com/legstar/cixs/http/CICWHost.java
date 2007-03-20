/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.cixs.http;

import java.io.IOException;
import com.legstar.host.HostContext;

/** 
 * This data object holds the characteristics of a given Host environment.
 * Properties are loaded from a Properties file or set individually. 
 * 
 * @author Fady Moussallam
 * 
 */
public class CICWHost {

	/** Label of Host IP address in properties file. */
	private static final String HOST_IP_ADDRESS = "HostIPAddress";
	
	/** Label of Host IP port in properties file. */
	private static final String HOST_IP_PORT = "HostIPPort";
	
	/** Label of Host user ID in properties file. */
	private static final String HOST_USER = "HostUser";
	
	/** Label of Host password in properties file. */
	private static final String HOST_PASSWORD = "HostPassword";
	
	/** Label of CICS HTTP Path in properties file. */
	private static final String HOST_PATH = "HostCICWPath";
	
	/** Label of Host character set name in properties file. */
	private static final String HOST_CHARSET_NAME = "HostCharsetName";
	
	/** TCPIP address of the Host. */
	private String mHostIPAddress;
	
	/** TCPIP port number on which the Host listens. */
	private int mHostIPPort;
	
	/** Host user ID for authentication. */
	private String mHostUser;
	
	/** Host password for authentication. */
	private String mHostPassword;
	
	/** The Path to the HTTP server on the Host. */
	private String mHostCICWPath;

	/** The host character set. */
	private String mHostCharsetName;

	/** Default no-arg constructor. */
	public CICWHost() {
		
	}

	/**
	 * Constructor from a properties file.
	 * @param propertiesFileName the name of the property file
	 * @throws IOException if property file cannot be read
	 */
	public CICWHost(final String propertiesFileName)
		throws IOException {
		
		/* load the program properties files */
		java.util.Properties hostProperties = new java.util.Properties();
		
		/* We use the thread loader rather than the class loader because this
		 * class will live in a different jar file than the calling
		 * application. */
		java.io.InputStream in =
			Thread.currentThread().getContextClassLoader().
			getResourceAsStream(propertiesFileName);
		if (in == null) {
			throw (new java.io.FileNotFoundException(propertiesFileName));
		}
		hostProperties.load(in);
		in.close();
		
		/* Set individual properties */
		mHostIPAddress = hostProperties.getProperty(HOST_IP_ADDRESS);
		mHostIPPort =
			Integer.parseInt(hostProperties.getProperty(HOST_IP_PORT));
		mHostUser = hostProperties.getProperty(HOST_USER);
		mHostPassword = hostProperties.getProperty(HOST_PASSWORD);
		mHostCICWPath = hostProperties.getProperty(HOST_PATH);
		mHostCharsetName = hostProperties.getProperty(HOST_CHARSET_NAME);
		if (mHostCharsetName == null || mHostCharsetName.length() == 0) {
			mHostCharsetName = HostContext.getDefaultHostCharsetName();
		}
	}

	/**
	 * Gets the Path to the HTTP server on the Host.
	 * @return the Path to the HTTP server
	 */
	public final String getHostCICWPath() {
		return mHostCICWPath;
	}

	/**
	 * Sets the path to the HTTP server on the Host.
	 * @param path the path to set
	 */
	public final void setHostCICWPath(final String path) {
		mHostCICWPath = path;
	}

	/**
	 * Gets the TCPIP address of the Host.
	 * @return the TCPIP address
	 */
	public final String getHostIPAddress() {
		return mHostIPAddress;
	}

	/**
	 * Sets the TCPIP address of the Host.
	 * @param address the TCPIP address to set
	 */
	public final void setHostIPAddress(final String address) {
		mHostIPAddress = address;
	}

	/**
	 * Gets the TCPIP port number on which the Host listens.
	 * @return the TCPIP address
	 */
	public final int getHostIPPort() {
		return mHostIPPort;
	}

	/**
	 * Sets the TCPIP port number on which the Host listens.
	 * @param port the TCPIP port to set
	 */
	public final void setHostIPPort(final int port) {
		mHostIPPort = port;
	}

	/** Gets the user ID used for host authentication/impersonation.
	 * @return host user ID
	 */
	public final String getHostUser() {
		return mHostUser;
	}

	/**
	 * Sets the user ID used for host authentication/impersonation.
	 * @param user host user ID to set
	 */
	public final void setHostUser(final String user) {
		mHostUser = user;
	}

	/** 
	 * Gets the password used for authentication.
	 * @return host user ID
	 */
	public final String getHostPassword() {
		return mHostPassword;
	}

	/**
	 * Sets the password used for authentication.
	 * @param password host user ID to set
	 */
	public final void setHostPassword(final String password) {
		mHostPassword = password;
	}

	/**
	 * @return the Host character set name
	 */
	public final String getHostCharsetName() {
		return mHostCharsetName;
	}

	/**
	 * @param hostCharsetName the Host character set name to set
	 */
	public final void setHostCharsetName(final String hostCharsetName) {
		mHostCharsetName = hostCharsetName;
	}

}
