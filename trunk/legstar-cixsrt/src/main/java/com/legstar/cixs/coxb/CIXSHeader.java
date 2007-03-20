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
package com.legstar.cixs.coxb;

/** 
 * This data object holds the data that clients want to pass on a request
 * by request basis. This is mainly used for credentials.
 * 
 * @author Fady Moussallam
 * 
 */
public class CIXSHeader {

	/** Host user ID for authentication. */
	private String mHostUser;
	
	/** Host password for authentication. */
	private String mHostPassword;
	
    /** TCPIP address of the Host. */
	private String hostIPAddress;

	/** TCPIP port number on which the Host listens. */
	private int hostIPPort = 0;

	/** The Path to the HTTP server on the Host. */
	private String hostCICWPath;
	    
	/** Default no-arg constructor. */
	public CIXSHeader() {
		
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
	 * Gets the TCPIP address of the Host.
	 * @return the TCPIP address
	 */
	public final String getHostIPAddress() {
		return hostIPAddress;
	}

	/**
	 * Sets the TCPIP address of the Host.
	 * @param address the TCPIP address to set
	 */
	public final void setHostIPAddress(final String address) {
		this.hostIPAddress = address;
	}

	/**
	 * Gets the TCPIP port number on which the Host listens.
	 * @return the TCPIP address
	 */
	public final int getHostIPPort() {
		return hostIPPort;
	}

	/**
	 * Sets the TCPIP port number on which the Host listens.
	 * @param port the TCPIP port to set
	 */
	public final void setHostIPPort(final int port) {
		this.hostIPPort = port;
	}

	/**
	 * Gets the Path to the HTTP server on the Host.
	 * @return the Path to the HTTP server
	 */
	public final String getHostCICWPath() {
		return hostCICWPath;
	}

	/**
	 * Sets the path to the HTTP server on the Host.
	 * @param path the path to set
	 */
	public final void setHostCICWPath(final String path) {
		this.hostCICWPath = path;
	}

}
