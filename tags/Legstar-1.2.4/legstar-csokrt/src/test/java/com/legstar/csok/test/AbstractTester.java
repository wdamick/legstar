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
package com.legstar.csok.test;

import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.config.Config;
import com.legstar.csok.client.CicsSocket;
import com.legstar.csok.client.CicsSocketEndpoint;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;

import junit.framework.TestCase;

/**
 * Generic test helper class.
 *
 */
public abstract class AbstractTester extends TestCase {

    /** Mainframe character set. */
    public static final String HOST_CHARSET = "IBM01140";
    
    /** Configuration file.*/
    public static final String CONFIG_FILE = "config.xml";
    
    /** Host user ID. */
    public static final String HOST_USERID = "STREAM2";
    
    /** A Socket endpoint. */
    private CicsSocketEndpoint mEndpoint;
    
    /** Address of target host. */
    private LegStarAddress mAddress;

    /** A socket connection to a mainframe. */
    private CicsSocket mConnection;
    
    /** Logger. */
    private static final Log LOG = LogFactory.getLog(AbstractTester.class);
   /**
     * @return the host connection
     */
    public CicsSocket getConnection() {
        return mConnection;
    }

    /**
     * @return the Address of target host
     */
    public LegStarAddress getAddress() {
        return mAddress;
    }

    /**
     * @return the Socket endpoint
     */
    public CicsSocketEndpoint getEndpoint() {
        return mEndpoint;
    }

    /**
     * Special setup using an endpoint configuration name.
     * @param endpointName endpoint name
     * @throws Exception if setup fails
     */
    public void setUp(final String endpointName) throws Exception {
        mEndpoint = new CicsSocketEndpoint(
                Config.loadEndpointConfiguration(CONFIG_FILE, endpointName));
        if (LOG.isDebugEnabled()) {
            mEndpoint.setHostTraceMode(true);
        }

        mAddress = new LegStarAddress(endpointName);
        mConnection = new CicsSocket(getName(), getEndpoint(), 1000, 5000);
        mConnection.connect(HOST_USERID);
    }

    /** {@inheritDoc} */
    public void tearDown() throws Exception {
        super.tearDown();
        mConnection.close();
    }

    /**
     * Create a request with a header built from a properties map.
     * @param map properties map for header creation
     * @return a legstar request
     * @throws RequestException if unable to create request
     */
    public LegStarRequest getRequest(final Map < String, Object > map) throws RequestException {
        try {
            LegStarMessage requestMessage = new LegStarMessage();
            requestMessage.getHeaderPart().setKeyValues(map);
            return new LegStarRequest(getName(), getAddress(), requestMessage);
        } catch (HeaderPartException e) {
            throw new RequestException(e);
        }
    }

}
