/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.http.client;

import com.legstar.messaging.ConnectionFactory;
import com.legstar.messaging.HostEndpoint;

/**
 * This class represents the parameters that are necessary for a client
 * to sucessfully connect to CICS over Http.
 * TODO Add proxy parameters.
 */
public class CicsHttpEndpoint extends HostEndpoint {

    /* ----------------------------------------------------------------------- */
    /* Member variables */
    /* ----------------------------------------------------------------------- */
    /** Support for http/https. */
    private String mHostURLProtocol = DEFAULT_HOST_URL_PROTOCOL;

    /** Host IP address. */
    private String mHostIPAddress;

    /** Host IP port. */
    private int mHostIPPort;

    /** Path to the HTTP server on the Host. */
    private String mHostURLPath = DEFAULT_HOST_URL_PATH;

    /* ----------------------------------------------------------------------- */
    /* Default values */
    /* ----------------------------------------------------------------------- */
    /** Default URL HTTP protocol. */
    private static final String DEFAULT_HOST_URL_PROTOCOL = "http";

    /** Default URL path to the CICS Http server program. */
    private static final String DEFAULT_HOST_URL_PATH = "/CICS/CWBA/LSWEBBIN";

    /** Query parm on URL tells the host to enter trace mode. */
    private static final String HOST_TRACE_QRY = "?trace";

    /** The default connection factory class. */
    private static final String DEFAULT_CONNECTION_FACTORY_CLASS =
            "com.legstar.http.client.CicsHttpConnectionFactory";

    /* ----------------------------------------------------------------------- */
    /* Labels */
    /* ----------------------------------------------------------------------- */
    /** Label for CICS HTTP Path. */
    private static final String HOST_URL_PATH_LABEL = "hostURLPath";

    /** Label for HTTP protocol. */
    private static final String HOST_URL_PROTOCOL_LABEL = "hostURLProtocol";

    /** Label for IP address. */
    private static final String IP_ADDRESS_LABEL = "hostIPAddress";

    /** Label for IP port. */
    private static final String IP_PORT_LABEL = "hostIPPort";

    /**
     * No-argument constructor.
     */
    public CicsHttpEndpoint() {
        setHostConnectionfactoryClass(DEFAULT_CONNECTION_FACTORY_CLASS);
    }

    /**
     * Constructor using an existing connection factory.
     * 
     * @param connectionFactory an instance of a connection factory
     */
    public CicsHttpEndpoint(final ConnectionFactory connectionFactory) {
        super(connectionFactory);
    }

    /**
     * Copy constructor.
     * 
     * @param copyFrom the endpoint to copy from
     */
    public CicsHttpEndpoint(final CicsHttpEndpoint copyFrom) {
        super(copyFrom);
        setHostIPAddress(copyFrom.getHostIPAddress());
        setHostIPPort(copyFrom.getHostIPPort());
        setHostURLPath(copyFrom.getHostURLPath());
        setHostURLProtocol(copyFrom.getHostURLProtocol());
    }

    /**
     * Perform a sanity check on the endpoint parameters.
     * 
     * @throws CicsHttpConnectionException if check fails
     */
    public void check() throws CicsHttpConnectionException {
        if (getHostIPAddress() == null || getHostIPAddress().length() == 0) {
            throw new CicsHttpConnectionException(
                    "No host IP address has been provided.");
        }
        if (getHostIPPort() == 0) {
            throw new CicsHttpConnectionException(
                    "No host IP port has been provided.");
        }
        if (getHostURLPath() == null || getHostURLPath().length() == 0) {
            throw new CicsHttpConnectionException(
                    "No host URL path has been provided.");
        }
    }

    /**
     * Helper to pretty print the endpoint content.
     * 
     * @return formatted endpoint report
     */
    public String toString() {
        String report = "CICS Http endpoint:"
                + super.toString()
                + "["
                + HOST_URL_PROTOCOL_LABEL + "=" + mHostURLProtocol
                + "," + IP_ADDRESS_LABEL + "=" + mHostIPAddress
                + "," + IP_PORT_LABEL + "=" + mHostIPPort
                + "," + HOST_URL_PATH_LABEL + "=" + mHostURLPath
                + "]";
        return report;
    }

    /**
     * @return the host IP address
     */
    public String getHostIPAddress() {
        return mHostIPAddress;
    }

    /**
     * @param hostIPAddress the host IP address to set
     */
    public void setHostIPAddress(final String hostIPAddress) {
        mHostIPAddress = hostIPAddress;
    }

    /**
     * @return the host IP port
     */
    public int getHostIPPort() {
        return mHostIPPort;
    }

    /**
     * @param hostIPPort the host IP port to set
     */
    public void setHostIPPort(final int hostIPPort) {
        mHostIPPort = hostIPPort;
    }

    /**
     * This method adds a query parm if host trace mode is on.
     * 
     * @return the path to the CICS Http server program
     */
    public String getHostURLPath() {
        if (isHostTraceMode() && !mHostURLPath.contains(HOST_TRACE_QRY)) {
            return mHostURLPath + HOST_TRACE_QRY;
        }
        return mHostURLPath;
    }

    /**
     * @param hostURLPath the path to the CICS Http server program to set
     */
    public void setHostURLPath(final String hostURLPath) {
        mHostURLPath = hostURLPath;
    }

    /**
     * @return the http protocol
     */
    public String getHostURLProtocol() {
        return mHostURLProtocol;
    }

    /**
     * @param hostURLProtocol the http protocol to set
     */
    public void setHostURLProtocol(final String hostURLProtocol) {
        mHostURLProtocol = hostURLProtocol;
    }

    /**
     * Not supported.
     * 
     * @param maxIdleTime the maximum time to keep a pooled connection opened
     *            to set -1 means forever.
     */
    @Override
    public void setPooledMaxIdleTime(final long maxIdleTime) {
        if (maxIdleTime != DEFAULT_POOLED_MAX_IDLE_TIME) {
            throw new IllegalArgumentException(
                    "pooledMaxIdleTime not supported by this transport");
        }
        super.setPooledMaxIdleTime(maxIdleTime);
    }

    /**
     * Not supported.
     * 
     * @param pooledMaxIdleTimeCheckPeriod the time between checking idle
     *            connections to set
     */
    @Override
    public void setPooledMaxIdleTimeCheckPeriod(
            final long pooledMaxIdleTimeCheckPeriod) {
        if (pooledMaxIdleTimeCheckPeriod != DEFAULT_POOLED_MAX_IDLE_TIME_CHECK_PERIOD) {
            throw new IllegalArgumentException(
                    "pooledMaxIdleTimeCheckPeriod not supported by this transport");
        }
        super.setPooledMaxIdleTimeCheckPeriod(pooledMaxIdleTimeCheckPeriod);
    }
}
