/*******************************************************************************
 * Copyright (c) 2009 LegSem.
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
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.ConnectionFactory;

/**
 * A concrete connection factory for CICS Http connections.
 */
public class CicsHttpConnectionFactory  implements ConnectionFactory {

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
     * Constructor for the CICS Http factory.
     * @param endpointConfig the an XML sub-hierarchy for an endpoint
     */
    public CicsHttpConnectionFactory(
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

        CicsHttp connection = new CicsHttp(
                connectionID,
                createEndPoint(address, mEndpointConfig),
                mEndpointConfig.getInt(
                        CONNECT_TIMEOUT_CFG, DEFAULT_CONNECT_TIMEOUT_MSEC),
                        mEndpointConfig.getInt(
                                RECEIVE_TIMEOUT_CFG, DEFAULT_READ_TIMEOUT_MSEC));


        return connection;
    }

    /**
     * Create a CICS Http endpoint by merging parameters from configuration
     * with parameters requested by the client in Address.
     * @param address the client endpoint parameters
     * @param endpointConfig the endpoint configuration
     * @return a CICS Http endpoint
     * @throws CicsHttpConnectionException if endpoint cannot be constructed
     */
    private CicsHttpEndpoint createEndPoint(
            final LegStarAddress address,
            final HierarchicalConfiguration endpointConfig)
    throws CicsHttpConnectionException {
        CicsHttpEndpoint cicsHttpEndpoint =
            new CicsHttpEndpoint(endpointConfig);

        /* If client is providing credentials, they take precedence over
         * the configuration parameters */
        if (address.getHostUserID() != null 
                && address.getHostUserID().length() > 0) {
            cicsHttpEndpoint.setHostUserID(address.getHostUserID());
        }
        if (address.getHostPassword() != null 
                && address.getHostPassword().length() > 0) {
            cicsHttpEndpoint.setHostPassword(address.getHostPassword());
        }
        if (address.getHostCharset() != null 
                && address.getHostCharset().length() > 0) {
            cicsHttpEndpoint.setHostCharset(address.getHostCharset());
        }
        if (address.isHostTraceMode()) {
            cicsHttpEndpoint.setHostTraceMode(true);
        }

        /* Check that this endpoint contains all mandatory parameters */
        if (cicsHttpEndpoint.getHostCharset() == null
                || cicsHttpEndpoint.getHostCharset().length() == 0) {
            throw new CicsHttpConnectionException(
            "No host character set has been provided.");
        }
        if (cicsHttpEndpoint.getHostIPAddress() == null
                || cicsHttpEndpoint.getHostIPAddress().length() == 0) {
            throw new CicsHttpConnectionException(
            "No host IP address has been provided.");
        }
        if (cicsHttpEndpoint.getHostIPPort() == 0) {
            throw new CicsHttpConnectionException(
            "No host IP port has been provided.");
        }
        if (cicsHttpEndpoint.getHostURLPath() == null
                || cicsHttpEndpoint.getHostURLPath().length() == 0) {
            throw new CicsHttpConnectionException(
            "No host URL path has been provided.");
        }


        return cicsHttpEndpoint;
    }


}
