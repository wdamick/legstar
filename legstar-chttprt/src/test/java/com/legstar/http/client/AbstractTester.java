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

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.config.Config;
import com.legstar.config.Constants;
import com.legstar.coxb.host.HostData;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.LsfileaeCases;

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
    
    /** Time out (in milliseconds) for initial connect. */
    public static final int DEFAULT_CONNECT_TIMEOUT_MSEC = 1000;
    
    /** Time out (in milliseconds) for read operations
     *  (waiting for host reply). */
    public static final int DEFAULT_READ_TIMEOUT_MSEC = 5000;

    /** A Http endpoint. */
    private CicsHttpEndpoint mEndpoint;
    
    /** Address of target host. */
    private LegStarAddress mAddress;

    /** A socket connection to a mainframe. */
    private CicsHttp mConnection;
    
    /** Logger. */
    private final Log _log = LogFactory.getLog(AbstractTester.class);
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
        mConnection = new CicsHttp(getName(), getEndpoint(), DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
        mConnection.setConnectTimeout(2000);
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

    /**
     * Create a request for LSFILEAE customer 100.
     * @return a request ready for execution
     * @throws RequestException if request cannot be built
     */
    public LegStarRequest getLsfileaeRequest100() throws RequestException {
        HashMap < String, Object > map = new HashMap < String, Object >();
        map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
        map.put(Constants.CICS_LENGTH_KEY, "79");
        map.put(Constants.CICS_DATALEN_KEY, "6");
        
        LegStarRequest request = getRequest(map);
        request.getRequestMessage().addDataPart(new CommareaPart(
                HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100())));
        return request;

    }
    /**
     * A request that is guaranteed to exceed 2 seconds.
     * @return a request ready for execution
     * @throws RequestException if request cannot be built
     */
    public LegStarRequest createLongRequest() throws RequestException {

        HashMap < String, Object> map = new HashMap < String, Object>();
        map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1SLEEPT");
        map.put(Constants.CICS_LENGTH_KEY, "39");
        map.put(Constants.CICS_DATALEN_KEY, "8");
        
        LegStarRequest request = getRequest(map);
        request.getRequestMessage().addDataPart(new CommareaPart(
                HostData.toByteArray("f0f0f0f0f0f0f0f3")));
        return request;
    }

    /**
     * A request that is guaranteed to fail (target program does not exist).
     * @return a request ready for execution
     * @throws RequestException if request cannot be built
     */
    public LegStarRequest createInvalidRequest() throws RequestException {

        HashMap < String, Object> map = new HashMap < String, Object>();
        map.put(Constants.CICS_PROGRAM_NAME_KEY, "TARATOZ");
        map.put(Constants.CICS_LENGTH_KEY, "79");
        map.put(Constants.CICS_DATALEN_KEY, "6");
        
        LegStarRequest request = getRequest(map);
        request.getRequestMessage().addDataPart(new CommareaPart(
                HostData.toByteArray("F0F0F0F1F0F0")));
        return request;

    }

}
