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
package com.legstar.mq.client;

import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.ConnectionFactory;

/**
 * A concrete connection factory for CICS MQ connections.
 */
public class CicsMQConnectionFactory  implements ConnectionFactory {

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
     * Constructor for the CICS MQ factory.
     * @param endpointConfig the an XML sub-hierarchy for an endpoint
     */
    public CicsMQConnectionFactory(
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

        CicsMQ connection = new CicsMQ(
                connectionID,
                createEndPoint(address, mEndpointConfig),
                mEndpointConfig.getInt(
                        CONNECT_TIMEOUT_CFG, DEFAULT_CONNECT_TIMEOUT_MSEC),
                        mEndpointConfig.getInt(
                                RECEIVE_TIMEOUT_CFG, DEFAULT_READ_TIMEOUT_MSEC));


        return connection;
    }

    /**
     * Create a CICS MQ endpoint by merging parameters from configuration
     * with parameters requested by the client in Address.
     * @param address the client endpoint parameters
     * @param endpointConfig the endpoint configuration
     * @return a CICS MQ endpoint
     * @throws CicsMQConnectionException if endpoint cannot be constructed
     */
    private CicsMQEndpoint createEndPoint(
            final LegStarAddress address,
            final HierarchicalConfiguration endpointConfig)
    throws CicsMQConnectionException {
        CicsMQEndpoint cicsMQEndpoint =
            new CicsMQEndpoint(endpointConfig);

        /* If client is providing credentials, they take precedence over
         * the configuration parameters */
        if (address.getHostUserID() != null 
                && address.getHostUserID().length() > 0) {
            cicsMQEndpoint.setHostUserID(address.getHostUserID());
        }
        if (address.getHostPassword() != null 
                && address.getHostPassword().length() > 0) {
            cicsMQEndpoint.setHostPassword(address.getHostPassword());
        }
        if (address.getHostCharset() != null 
                && address.getHostCharset().length() > 0) {
            cicsMQEndpoint.setHostCharset(address.getHostCharset());
        }
        if (address.isHostTraceMode()) {
            cicsMQEndpoint.setHostTraceMode(true);
        }

        /* Check that this endpoint contains all mandatory parameters */
        if (cicsMQEndpoint.getHostCharset() == null
                || cicsMQEndpoint.getHostCharset().length() == 0) {
            throw new CicsMQConnectionException(
            "No host character set has been provided.");
        }
        if (cicsMQEndpoint.getHostIPAddress() == null
                || cicsMQEndpoint.getHostIPAddress().length() == 0) {
            throw new CicsMQConnectionException(
            "No host IP address has been provided.");
        }
        if (cicsMQEndpoint.getHostIPPort() == 0) {
            throw new CicsMQConnectionException(
            "No host IP port has been provided.");
        }
        if (cicsMQEndpoint.getHostMQManager() == null
                || cicsMQEndpoint.getHostMQManager().length() == 0) {
            throw new CicsMQConnectionException(
            "No host MQ Manager name has been provided.");
        }
        if (cicsMQEndpoint.getHostMQChannel() == null
                || cicsMQEndpoint.getHostMQChannel().length() == 0) {
            throw new CicsMQConnectionException(
            "No host MQ Channel name has been provided.");
        }
        if (cicsMQEndpoint.getHostMQRequestQueue() == null
                || cicsMQEndpoint.getHostMQRequestQueue().length() == 0) {
            throw new CicsMQConnectionException(
            "No host MQ Request queue name has been provided.");
        }
        if (cicsMQEndpoint.getHostMQResponseQueue() == null
                || cicsMQEndpoint.getHostMQResponseQueue().length() == 0) {
            throw new CicsMQConnectionException(
            "No host MQ Response queue name has been provided.");
        }


        return cicsMQEndpoint;
    }


}
