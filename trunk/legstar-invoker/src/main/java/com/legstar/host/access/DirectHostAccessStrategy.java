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
package com.legstar.host.access;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.messaging.Connection;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.ConnectionFactory;
import com.legstar.messaging.Request;
import com.legstar.messaging.RequestException;
import com.legstar.config.Config;

/**
 * This transport-independant accessor dynamically loads a connection
 * factory and issues a direct synchronous call to a host.
 */
public class DirectHostAccessStrategy implements HostAccessStrategy {

	/** Logger. */
	private static final Log LOG =
		LogFactory.getLog(DirectHostAccessStrategy.class);
	
	/** The connection factory is dynamically loaded. */
	private ConnectionFactory mConnectionFactory;
	
	/**
	 * Construct a direct host accessor from an in-memory configuration xml.
	 * @param endpointConfig an XML configuration sub-hierarchy for an endpoint
	 * @throws HostAccessStrategyException if connection factory cannot
	 *  be created
	 */
	public DirectHostAccessStrategy(
			final HierarchicalConfiguration endpointConfig)
			throws HostAccessStrategyException {
		endpointConfig.setExpressionEngine(new XPathExpressionEngine());
		try {
			mConnectionFactory = Config.loadConnectionFactory(endpointConfig);
		} catch (ConfigurationException e) {
			throw new HostAccessStrategyException(e);
		}
	}
	
	/** (non-Javadoc).
	 * @see com.legstar.host.access.HostAccessStrategy#invoke(
	 * com.legstar.messaging.Request)
	 * {@inheritDoc}
	 */
	public final void invoke(
			final Request request) throws HostAccessStrategyException {
		
		long startTime = System.currentTimeMillis();
		if (LOG.isDebugEnabled()) {
			LOG.debug("Direct invoke for Request:" + request.getID());
		}
		try {
			Connection connection = mConnectionFactory.createConnection(
					request.getID(), request.getAddress());
			connection.connect(request.getAddress().getHostPassword());
			connection.sendRequest(request);
			connection.recvResponse(request);
			connection.close();
		} catch (ConnectionException e) {
			request.setException(new RequestException(e));
			throw new HostAccessStrategyException(e);
		} catch (RequestException e) {
			request.setException(new RequestException(e));
			throw new HostAccessStrategyException(e);
		}
		if (LOG.isDebugEnabled()) {
			long endTime = System.currentTimeMillis();
			LOG.debug("Direct invoke for Request:" + request.getID()
					+ " ended. elapse: "
					+ Long.toString(endTime - startTime) + " ms");
		}
	}

}
