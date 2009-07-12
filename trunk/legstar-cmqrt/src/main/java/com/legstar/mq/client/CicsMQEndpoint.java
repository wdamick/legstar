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
package com.legstar.mq.client;

import com.legstar.messaging.ConnectionFactory;
import com.legstar.messaging.HostEndpoint;

/**
 * This class represents the parameters that are necessary for a client
 * to successfully connect to CICS over MQ.
 */
public class CicsMQEndpoint extends HostEndpoint {

    /* ----------------------------------------------------------------------- */
    /* Member variables                                                        */
    /* ----------------------------------------------------------------------- */
    /** Host IP address. */
    private String mHostIPAddress;

    /** Host IP port. */
    private int mHostIPPort = DEFAULT_MQ_PORT;

    /** Host MQ Manager. */
    private String mHostMQManager = DEFAULT_MQ_MANAGER;

    /** Host MQ Channel. */
    private String mHostMQChannel;

    /** Host MQ Request queue name. */
    private String mHostMQRequestQueue;

    /** Host MQ Reply queue name. */
    private String mHostMQResponseQueue;

    /** Host MQ Bridge implementation. */
    private HostMQBridgeType mHostMQBridgeType = HostMQBridgeType.LSMSG;

    /* ----------------------------------------------------------------------- */
    /* Default values                                                          */
    /* ----------------------------------------------------------------------- */
    /** Default MQ Manager. */
    private static final String DEFAULT_MQ_MANAGER = "CSQ1";

    /** Default MQ IP port. */
    private static final int DEFAULT_MQ_PORT = 1414;

    /** The default connection factory class. */
    private static final String DEFAULT_CONNECTION_FACTORY_CLASS =
        "com.legstar.mq.client.CicsMQConnectionFactory";

    /* ----------------------------------------------------------------------- */
    /* Labels                                                                  */
    /* ----------------------------------------------------------------------- */
    /** Label for IP address. */
    private static final String IP_ADDRESS_LABEL = "hostIPAddress";

    /** Label for IP port. */
    private static final String IP_PORT_LABEL = "hostIPPort";

    /** Label for MQ Manager. */
    private static final String HOST_MQ_MANAGER_LABEL = "hostMQManager";

    /** Label for MQ Channel. */
    private static final String HOST_MQ_CHANNEL_LABEL = "hostMQChannel";

    /** Label for MQ Request queue. */
    private static final String HOST_MQ_REQUEST_Q_LABEL = "hostMQRequestQueue";

    /** Label for MQ Reply queue. */
    private static final String HOST_MQ_RESPONSE_Q_LABEL = "hostMQResponseQueue";

    /** Label for host MQ bridge type. */
    private static final String HOST_MQ_BRIDGE_TYPE_LABEL = "hostMQBridgeType";

    /**
     * No-arg constructor.
     */
    public CicsMQEndpoint() {
        setHostConnectionfactoryClass(DEFAULT_CONNECTION_FACTORY_CLASS);
    }

    /**
     * Constructor using an existing connection factory.
     * @param connectionFactory an instance of a connection factory
     */
    public CicsMQEndpoint(final ConnectionFactory connectionFactory) {
        super(connectionFactory);
    }

    /**
     * Copy constructor.
     * @param copyFrom the endpoint to copy from
     */
    public CicsMQEndpoint(final CicsMQEndpoint copyFrom) {
        super(copyFrom);
        setHostIPAddress(copyFrom.getHostIPAddress());
        setHostIPPort(copyFrom.getHostIPPort());
        setHostMQBridgeType(copyFrom.getHostMQBridgeType());
        setHostMQChannel(copyFrom.getHostMQChannel());
        setHostMQRequestQueue(copyFrom.getHostMQRequestQueue());
        setHostMQResponseQueue(copyFrom.getHostMQResponseQueue());
        setHostMQManager(copyFrom.getHostMQManager());
    }

    /**
     * Helper to pretty print the endpoint content.
     * @return formatted endpoint report
     */
    public String toString() {
        String report = "CICS WMQ endpoint:"
            + super.toString()
            + "[" 
            + IP_ADDRESS_LABEL + "=" + mHostIPAddress
            + "," + IP_PORT_LABEL + "=" + mHostIPPort
            + "," + HOST_MQ_MANAGER_LABEL + "=" + mHostMQManager
            + "," + HOST_MQ_CHANNEL_LABEL + "=" + mHostMQChannel
            + "," + HOST_MQ_REQUEST_Q_LABEL + "=" + mHostMQRequestQueue
            + "," + HOST_MQ_RESPONSE_Q_LABEL + "=" + mHostMQResponseQueue
            + "," + HOST_MQ_BRIDGE_TYPE_LABEL + "=" + mHostMQBridgeType
            + "]";
        return report;
    }

    /**
     * Perform a sanity check on the endpoint parameters.
     * @throws CicsMQConnectionException if check fails
     */
    public void check() throws CicsMQConnectionException {
        if (getHostIPAddress() == null || getHostIPAddress().length() == 0) {
            throw new CicsMQConnectionException(
            "No host IP address has been provided.");
        }
        if (getHostIPPort() == 0) {
            throw new CicsMQConnectionException(
            "No host IP port has been provided.");
        }
        if (getHostMQManager() == null
                || getHostMQManager().length() == 0) {
            throw new CicsMQConnectionException(
            "No host MQ Manager name has been provided.");
        }
        if (getHostMQChannel() == null
                || getHostMQChannel().length() == 0) {
            throw new CicsMQConnectionException(
            "No host MQ Channel name has been provided.");
        }
        if (getHostMQRequestQueue() == null
                || getHostMQRequestQueue().length() == 0) {
            throw new CicsMQConnectionException(
            "No host MQ Request queue name has been provided.");
        }
        if (getHostMQResponseQueue() == null
                || getHostMQResponseQueue().length() == 0) {
            throw new CicsMQConnectionException(
            "No host MQ Response queue name has been provided.");
        }
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

    /**
     * Types of mainframe MQ Bridge implementations supported.
     */
    public enum HostMQBridgeType {
        /** Uses LegStar messaging (expects mainframe to process LegStar messages). */
        LSMSG, 
        /** Uses IBM CICS MQ Bridge. */
        MQCIH       

    }

    /**
     * @return the Host MQ Bridge implementation
     */
    public HostMQBridgeType getHostMQBridgeType() {
        return mHostMQBridgeType;
    }

    /**
     * @param hostMQBridgeType the Host MQ Bridge implementation to set
     */
    public void setHostMQBridgeType(final HostMQBridgeType hostMQBridgeType) {
        mHostMQBridgeType = hostMQBridgeType;
    }

    /**
     * @param hostMQBridgeType the Host MQ Bridge implementation to set
     */
    public void setHostMQBridgeType(final String hostMQBridgeType) {
        mHostMQBridgeType = HostMQBridgeType.valueOf(hostMQBridgeType);
    }
}
