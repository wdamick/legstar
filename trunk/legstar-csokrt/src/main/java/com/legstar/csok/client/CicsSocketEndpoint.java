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
package com.legstar.csok.client;

import com.legstar.messaging.ConnectionFactory;
import com.legstar.messaging.HostEndpoint;


/**
 * This class represents the parameters that are necessary for a client
 * to successfully connect to CICS over sockets.
 */
public class CicsSocketEndpoint extends HostEndpoint {

    /* ----------------------------------------------------------------------- */
    /* Member variables                                                        */
    /* ----------------------------------------------------------------------- */
    /** Host IP address. */
    private String mHostIPAddress;

    /** Host IP port. */
    private int mHostIPPort;

    /* ----------------------------------------------------------------------- */
    /* Default values                                                          */
    /* ----------------------------------------------------------------------- */
    /** The default connection factory class. */
    private static final String DEFAULT_CONNECTION_FACTORY_CLASS =
        "com.legstar.csok.client.CicsSocketConnectionFactory";

    /* ----------------------------------------------------------------------- */
    /* Labels                                                                  */
    /* ----------------------------------------------------------------------- */
    /** Label for IP address. */
    private static final String IP_ADDRESS_LABEL = "hostIPAddress";

    /** Label for IP port. */
    private static final String IP_PORT_LABEL = "hostIPPort";

    /**
     * No-argument constructor.
     */
    public CicsSocketEndpoint() {
        setHostConnectionfactoryClass(DEFAULT_CONNECTION_FACTORY_CLASS);
    }
    
    /**
     * Constructor using an existing connection factory.
     * @param connectionFactory an instance of a connection factory
     */
    public CicsSocketEndpoint(final ConnectionFactory connectionFactory) {
        super(connectionFactory);
    }

    /**
     * Copy constructor.
     * @param copyFrom the endpoint to copy from
     */
    public CicsSocketEndpoint(final CicsSocketEndpoint copyFrom) {
        super(copyFrom);
        setHostIPAddress(copyFrom.getHostIPAddress());
        setHostIPPort(copyFrom.getHostIPPort());
    }

    /**
     * Perform a sanity check on the endpoint parameters.
     * @throws CicsSocketConnectionException if check fails
     */
    public void check() throws CicsSocketConnectionException {
        if (getHostIPAddress() == null || getHostIPAddress().length() == 0) {
            throw new CicsSocketConnectionException(
            "No host IP address has been provided.");
        }
        if (getHostIPPort() == 0) {
            throw new CicsSocketConnectionException(
            "No host IP port has been provided.");
        }
    }

    /**
     * Helper to pretty print the endpoint content.
     * @return formatted endpoint report
     */
    public String toString() {
        String report = "CICS Http endpoint:"
            + super.toString()
            + "[" 
            + IP_ADDRESS_LABEL + "=" + mHostIPAddress
            + "," + IP_PORT_LABEL + "=" + mHostIPPort
            + "]";
        return report;
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

}
