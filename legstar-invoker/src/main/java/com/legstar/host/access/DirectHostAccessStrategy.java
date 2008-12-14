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
package com.legstar.host.access;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.ConnectionFactory;
import com.legstar.messaging.LegStarRequest;
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
            final LegStarRequest request) throws HostAccessStrategyException {

        long startTime = System.currentTimeMillis();
        if (LOG.isDebugEnabled()) {
            LOG.debug("Direct invoke for Request:" + request.getID());
        }
        try {
            LegStarConnection connection = mConnectionFactory.createConnection(
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
