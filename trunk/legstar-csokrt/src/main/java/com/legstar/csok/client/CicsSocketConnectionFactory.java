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
package com.legstar.csok.client;

import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.ConnectionFactory;

/**
 * A concrete connection factory for CICS Socket connections.
 */
public class CicsSocketConnectionFactory implements ConnectionFactory {

    /** Configuration XPath location for socket connect timeout. */
    private static final String CONNECT_TIMEOUT_CFG =
        "connectTimeout";

    /** Configuration XPath location for socket receive timeout. */
    private static final String RECEIVE_TIMEOUT_CFG =
        "receiveTimeout";

    /** Time out (in milliseconds) for initial connect. */
    private static final int DEFAULT_CONNECT_TIMEOUT_MSEC = 1000;

    /** Time out (in milliseconds) for read operations
     *  (waiting for host reply). */
    private static final int DEFAULT_READ_TIMEOUT_MSEC = 5000;

    /** Configuration for an endpoint. */
    private HierarchicalConfiguration mEndpointConfig;

    /**
     * Constructor for the CICS Socket factory.
     * @param endpointConfig the an XML sub-hierarchy for an endpoint
     */
    public CicsSocketConnectionFactory(
            final HierarchicalConfiguration endpointConfig) {
        mEndpointConfig = endpointConfig;
        mEndpointConfig.setExpressionEngine(new XPathExpressionEngine());
    }

    /** (non-Javadoc).
     * @see com.legstar.messaging.ConnectionFactory#createConnection(
     * java.lang.String, com.legstar.messaging.LegStarAddress)
     * {@inheritDoc}
     */
    public final LegStarConnection createConnection(
            final String connectionID,
            final LegStarAddress address) throws ConnectionException {

        CicsSocket connection = new CicsSocket(
                connectionID, createEndPoint(address, mEndpointConfig),
                mEndpointConfig.getInt(
                        CONNECT_TIMEOUT_CFG, DEFAULT_CONNECT_TIMEOUT_MSEC),
                        mEndpointConfig.getInt(
                                RECEIVE_TIMEOUT_CFG, DEFAULT_READ_TIMEOUT_MSEC));


        return connection;
    }

    /**
     * Create a CICS Socket endpoint by merging parameters from configuration
     * with parameters requested by the client in Address.
     * @param address the client endpoint parameters
     * @param endpointConfig the endpoint configuration
     * @return a CICS Socket endpoint
     * @throws CicsSocketConnectionException if endpoint cannot be constructed
     */
    private CicsSocketEndpoint createEndPoint(
            final LegStarAddress address,
            final HierarchicalConfiguration endpointConfig)
    throws CicsSocketConnectionException {
        CicsSocketEndpoint cicsSocketEndpoint =
            new CicsSocketEndpoint(endpointConfig);

        /* If client is providing credentials, they take precedence over
         * the configuration parameters */
        if (address.getHostUserID() != null 
                && address.getHostUserID().length() > 0) {
            cicsSocketEndpoint.setHostUserID(address.getHostUserID());
        }
        if (address.getHostPassword() != null 
                && address.getHostPassword().length() > 0) {
            cicsSocketEndpoint.setHostPassword(address.getHostPassword());
        }
        if (address.getHostCharset() != null 
                && address.getHostCharset().length() > 0) {
            cicsSocketEndpoint.setHostCharset(address.getHostCharset());
        }
        if (address.isHostTraceMode()) {
            cicsSocketEndpoint.setHostTraceMode(true);
        }

        /* Check that this endpoint contains all mandatory parameters */
        if (cicsSocketEndpoint.getHostCharset() == null
                || cicsSocketEndpoint.getHostCharset().length() == 0) {
            throw new CicsSocketConnectionException(
            "No host character set has been provided.");
        }
        if (cicsSocketEndpoint.getHostUserID() == null
                || cicsSocketEndpoint.getHostUserID().length() == 0) {
            throw new CicsSocketConnectionException(
            "No host user ID has been provided.");
        }
        if (cicsSocketEndpoint.getHostIPAddress() == null
                || cicsSocketEndpoint.getHostIPAddress().length() == 0) {
            throw new CicsSocketConnectionException(
            "No host IP address has been provided.");
        }
        if (cicsSocketEndpoint.getHostIPPort() == 0) {
            throw new CicsSocketConnectionException(
            "No host IP port has been provided.");
        }

        return cicsSocketEndpoint;
    }

}
