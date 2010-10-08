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
package com.legstar.csok.test;

import java.io.UnsupportedEncodingException;
import java.util.HashMap;

import com.legstar.config.Constants;
import com.legstar.coxb.host.HostData;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.test.coxb.T1volumeCases;

/**
 * Get a sense of performances in various scenario.
 *
 */
public class VolumeTest extends AbstractSocketConnectionTester {

    /** Maximum number of iterations. */
    private static final int MAX_ITERATIONS = 5;

    /** {@inheritDoc} */
    public void setUp() throws Exception {
        super.setUp("CICSTS23");
        getEndpoint().setHostTraceMode(false); // dont flood the host
    }
    /** 
     * Single thread iterating through simple requests with wait time.
     */
    public void testSingleIterateSimpleWithWait() {
        long startTime = System.currentTimeMillis();
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1SLEEPT");
            map.put(Constants.CICS_LENGTH_KEY, "39");
            map.put(Constants.CICS_DATALEN_KEY, "8");
            
            LegStarRequest request = getRequest(map, getAddress());
            request.getRequestMessage().addDataPart(new CommareaPart(HostData.toByteArray("f0f0f0f0f0f0f0f3")));
            
            for (int i = 0; i < MAX_ITERATIONS; i++) {
                getConnection().sendRequest(request);
                getConnection().recvResponse(request);
                getConnection().keepUOW();
                String reply = new String(request.getResponseMessage().getDataParts().get(0).getContent(),
                        HOST_CHARSET);
                assertEquals("00000003P390    LEG1", reply.substring(0, 20));
            }
            long endTime = System.currentTimeMillis();
            System.out.println("Duration millisecs=" + (endTime - startTime));
        } catch (RequestException e) {
            fail("testSingleIterateSimpleWithWait failed=" + e);
        } catch (UnsupportedEncodingException e) {
            fail("testSingleIterateSimpleWithWait failed=" + e);
        }
    }

    /**
     * Single thread iterating through simple requests reusing the same connection.
     */
    public void testSingleIterateSimpleConnectionReused() {
        long startTime = System.currentTimeMillis();
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
            map.put(Constants.CICS_LENGTH_KEY, "79");
            map.put(Constants.CICS_DATALEN_KEY, "6");
            
            LegStarRequest request = getRequest(map, getAddress());
            request.getRequestMessage().addDataPart(new CommareaPart(
                    HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100())));
            for (int i = 0; i < MAX_ITERATIONS; i++) {
                getConnection().sendRequest(request);
                getConnection().recvResponse(request);
                getConnection().commitUOW();
                assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                        HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
            }
            long endTime = System.currentTimeMillis();
            System.out.println("Duration millisecs=" + (endTime - startTime));
        } catch (RequestException e) {
            fail("testSingleIterateSimpleConnectionReused failed=" + e);
        }
    }

    /** 
     * Single thread iterating through simple requests, changing connection each time.
     */
    public void testSingleIterateSimple() {
        long startTime = System.currentTimeMillis();
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
            map.put(Constants.CICS_LENGTH_KEY, "79");
            map.put(Constants.CICS_DATALEN_KEY, "6");
            
            LegStarRequest request = getRequest(map, getAddress());
            request.getRequestMessage().addDataPart(new CommareaPart(
                    HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100())));
            for (int i = 0; i < MAX_ITERATIONS; i++) {
                getConnection().connect(HOST_PASSWORD);
                getConnection().sendRequest(request);
                getConnection().recvResponse(request);
                getConnection().commitUOW();
                getConnection().close();
                assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                        HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
            }
            long endTime = System.currentTimeMillis();
            System.out.println("Duration millisecs=" + (endTime - startTime));
        } catch (ConnectionException e) {
            fail("testSingleIterateSimple failed=" + e);
        } catch (RequestException e) {
            fail("testSingleIterateSimple failed=" + e);
        }
    }

    /**
     * Single thread iterates thru large data requests.
     */
    public void testSingleIterateVolume() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1VOLUME");
            map.put(Constants.CICS_LENGTH_KEY, "32767");
            map.put(Constants.CICS_DATALEN_KEY, "32767");

            LegStarRequest request = getRequest(map, getAddress());
            request.getRequestMessage().addDataPart(
                    new CommareaPart(T1volumeCases.getHostBytes(32767)));
            for (int i = 0; i < MAX_ITERATIONS; i++) {
                getConnection().sendRequest(request);
                getConnection().recvResponse(request);
                getConnection().keepUOW();
                T1volumeCases.checkByteArray(request.getResponseMessage().getDataParts().get(0).getContent());
            }
        } catch (RequestException e) {
            e.printStackTrace();
            fail("testSingleIterateVolume failed=" + e);
        }
    }

}
