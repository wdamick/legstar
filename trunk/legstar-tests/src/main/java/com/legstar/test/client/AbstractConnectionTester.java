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
package com.legstar.test.client;

import java.rmi.server.UID;
import java.util.HashMap;
import java.util.Map;

import com.legstar.config.Constants;
import com.legstar.coxb.host.HostData;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.test.coxb.T1volumeCases;

import junit.framework.TestCase;

/**
 * Generic test helper class.
 *
 */
public abstract class AbstractConnectionTester extends TestCase {

    /** Mainframe character set. */
    public static final String HOST_CHARSET = "IBM01140";
    
    /** Configuration file.*/
    public static final String CONFIG_FILE = "config.xml";
    
    /** Host user ID. */
    public static final String HOST_USERID = "P390";
    
    /** Host user ID. */
    public static final String HOST_PASSWORD = "STREAM2";
    
    /** Time out (in milliseconds) for initial connect. */
    public static final int DEFAULT_CONNECT_TIMEOUT_MSEC = 1000;
    
    /** Time out (in milliseconds) for read operations
     *  (waiting for host reply). */
    public static final int DEFAULT_READ_TIMEOUT_MSEC = 5000;

    /**
     * Create a request with a header built from a properties map.
     * @param map properties map for header creation
     * @param address the destination
     * @return a legstar request
     * @throws RequestException if unable to create request
     */
    public static LegStarRequest getRequest(
            final Map < String, Object > map,
            final LegStarAddress address) throws RequestException {
        try {
            LegStarMessage requestMessage = new LegStarMessage();
            requestMessage.getHeaderPart().setKeyValues(map);
            /* With MQ, the connection ID is used as the correlation ID. It is
             * important that it is unique for each request.*/
            String uid = new UID().toString();
            String[] comps = uid.split(":");
            String requestID = comps[1] + comps[2];
           return new LegStarRequest(requestID, address, requestMessage);
        } catch (HeaderPartException e) {
            throw new RequestException(e);
        }
    }

    /**
     * Create a request for LSFILEAE customer 100.
     * @param address the destination
     * @return a request ready for execution
     * @throws RequestException if request cannot be built
     */
    public static LegStarRequest getLsfileaeRequest100(
            final LegStarAddress address) throws RequestException {
        HashMap < String, Object > map = new HashMap < String, Object >();
        map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
        map.put(Constants.CICS_LENGTH_KEY, "79");
        map.put(Constants.CICS_DATALEN_KEY, "6");
        
        LegStarRequest request = getRequest(map, address);
        request.getRequestMessage().addDataPart(new CommareaPart(
                HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100())));
        return request;

    }
    /**
     * A request that is guaranteed to exceed 2 seconds.
     * @param address the destination
     * @return a request ready for execution
     * @throws RequestException if request cannot be built
     */
    public static LegStarRequest createLongRequest(
            final LegStarAddress address) throws RequestException {
        return createLongRequest(address, "f0f0f0f0f0f0f0f3");
    }

    /**
     * A request that will wait depending on the parameter passed.
     * @param address the destination
     * @param hostByteHex wait time as a hex string of a display numeric
     * @return a request ready for execution
     * @throws RequestException if request cannot be built
     */
    public static LegStarRequest createLongRequest(
            final LegStarAddress address, final String hostByteHex) throws RequestException {

        HashMap < String, Object> map = new HashMap < String, Object>();
        map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1SLEEPT");
        map.put(Constants.CICS_LENGTH_KEY, "39");
        map.put(Constants.CICS_DATALEN_KEY, "8");
        
        LegStarRequest request = getRequest(map, address);
        request.getRequestMessage().addDataPart(new CommareaPart(
                HostData.toByteArray(hostByteHex)));
        return request;
    }
    /**
     * A request that is guaranteed to fail (target program does not exist).
     * @param address the destination
     * @return a request ready for execution
     * @throws RequestException if request cannot be built
     */
    public static LegStarRequest createInvalidRequest(
            final LegStarAddress address) throws RequestException {

        HashMap < String, Object> map = new HashMap < String, Object>();
        map.put(Constants.CICS_PROGRAM_NAME_KEY, "TARATOZ");
        map.put(Constants.CICS_LENGTH_KEY, "79");
        map.put(Constants.CICS_DATALEN_KEY, "6");
        
        LegStarRequest request = getRequest(map, address);
        request.getRequestMessage().addDataPart(new CommareaPart(
                HostData.toByteArray("F0F0F0F1F0F0")));
        return request;

    }
    
    /**
     * A request with a large payload.
     * @param address the destination
     * @return a request ready for execution
     * @throws RequestException if request cannot be built
     */
    public static LegStarRequest createLargeRequest(
            final LegStarAddress address) throws RequestException {
        HashMap < String, Object > map = new HashMap < String, Object >();
        map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1VOLUME");
        map.put(Constants.CICS_LENGTH_KEY, "32767");
        map.put(Constants.CICS_DATALEN_KEY, "32767");

        LegStarRequest request = getRequest(map, address);
        request.getRequestMessage().addDataPart(
                new CommareaPart(T1volumeCases.getHostBytes(32767)));
        return request;
    }

    /**
     * A request with a large payload.
     * @param address the destination
     * @return a request ready for execution
     * @throws RequestException if request cannot be built
     */
    public static LegStarRequest createLargeRequestB(
            final LegStarAddress address) throws RequestException {
        HashMap < String, Object > map = new HashMap < String, Object >();
        map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1VOLUMB");
        map.put(Constants.CICS_LENGTH_KEY, "32759");
        map.put(Constants.CICS_DATALEN_KEY, "32759");

        LegStarRequest request = getRequest(map, address);
        request.getRequestMessage().addDataPart(
                new CommareaPart(T1volumeCases.getHostBytes(32759)));
        return request;
    }
}
