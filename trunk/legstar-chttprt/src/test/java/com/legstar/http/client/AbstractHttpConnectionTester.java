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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.config.Config;
import com.legstar.messaging.LegStarAddress;
import com.legstar.test.client.AbstractConnectionTester;

/**
 * Generic test helper class.
 *
 */
public abstract class AbstractHttpConnectionTester extends AbstractConnectionTester {

    /** Configuration file.*/
    public static final String CONFIG_FILE = "config.xml";
    
    /** A Http endpoint. */
    private CicsHttpEndpoint mEndpoint;
    
    /** Address of target host. */
    private LegStarAddress mAddress;

    /** A socket connection to a mainframe. */
    private CicsHttp mConnection;
    
    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());
   /**
     * @return the host connection
     */
    public CicsHttp getConnection() {
        return mConnection;
    }

    /**
     * @return the Address of target host
     */
    public LegStarAddress getAddress() {
        return mAddress;
    }

    /**
     * @return the Http endpoint
     */
    public CicsHttpEndpoint getEndpoint() {
        return mEndpoint;
    }

    /**
     * Special setup using an endpoint configuration name.
     * @param endpointName endpoint name
     * @throws Exception if setup fails
     */
    public void setUp(final String endpointName) throws Exception {
        mEndpoint = new CicsHttpEndpoint(
                Config.loadEndpointConfiguration(CONFIG_FILE, endpointName));
        if (_log.isDebugEnabled()) {
            mEndpoint.setHostTraceMode(true);
        }

        mAddress = new LegStarAddress(endpointName);
        mConnection = new CicsHttp(getName(), getEndpoint(),
                DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
        mConnection.setConnectTimeout(2000);
        mConnection.connect(HOST_USERID);
    }

    /** {@inheritDoc} */
    public void tearDown() throws Exception {
        super.tearDown();
        mConnection.close();
    }

}
