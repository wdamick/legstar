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

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.MultiThreadedHttpConnectionManager;

/**
 * This is  factory of connections.
 * The apache HTTP client documentation recommends that the connection
 * manager and http client be singletons
 *
 * @author Fady Moussallam
 * 
 */
public class CICWConnectionFactory {
	
	/** Apache HTTP connection manager. */
	private static MultiThreadedHttpConnectionManager mConnectionManager = null;

	/** Apache HTTP client instance. */
	private static HttpClient mHttpClient = null;
	
	/** Default maximum number of connections per host. */
	private static final int DEFAULT_MAX_CONNECTIONS_PER_HOST = 20;
	
	/** Default maximum total number of connections. */
	private static final int DEFAULT_MAX_TOTAL_CONNECTIONS = 100;
	
	/**
	 *  General constructor.
	 */
	public CICWConnectionFactory() {
		if (mConnectionManager == null) {
			createManager();
		}
	}
	
	/**
	 * Create the connection manager.
	 */
	public final synchronized void createManager() {
		if (mConnectionManager == null) {
			mConnectionManager = new MultiThreadedHttpConnectionManager();
			mConnectionManager.getParams().setMaxTotalConnections(
					DEFAULT_MAX_TOTAL_CONNECTIONS);
			mConnectionManager.getParams().setDefaultMaxConnectionsPerHost(
					DEFAULT_MAX_CONNECTIONS_PER_HOST);
			mHttpClient = new HttpClient();
			mHttpClient.setHttpConnectionManager(mConnectionManager);
			/* Force credentials to be sent along with the request
			* This avoids a challenge/response */
			mHttpClient.getParams().setAuthenticationPreemptive(true);
		}
	}

	/**
	 * Shutdown the connection manager.
	 */
	public final synchronized void shutdown() {
		if (mConnectionManager != null) {
			mConnectionManager.shutdown();
			mConnectionManager = null;
			mHttpClient = null;
		}
	}
	
	/**
	 * Create a new connection object given host parameters.
	 * 
	 * @param host The host connection parameters
	 * @return Returns a connection object
	 */
	public final CICWConnection createConnection(final CICWHost host) {
		return new CICWConnection(host, mHttpClient);		
	}
	
}
