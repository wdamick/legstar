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

import java.util.HashMap;

import com.legstar.config.Constants;
import com.legstar.coxb.host.HostData;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.ContainerPart;
import com.legstar.messaging.LegStarMessagePart;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.LsfileacCases;
import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.test.coxb.T1volumeCases;

/** 
 * Test the Socket transport against CICS TS 3.1.
 */
public class CicsTs31SocketTest extends AbstractTester {

    /** This is for the memory stress test. */
    private static final int MAX_ITERATIONS = 100;

    /** {@inheritDoc} */
    public void setUp() throws Exception {
        super.setUp("CICSTS31");
    }

    /**
     * Test a commarea based program.
     */
    public void testCommareaProgram() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
            map.put(Constants.CICS_LENGTH_KEY, "79");
            map.put(Constants.CICS_DATALEN_KEY, "6");
            
            LegStarRequest request = getRequest(map);
            request.getRequestMessage().addDataPart(new CommareaPart(
                    HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100())));
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
            getConnection().commitUOW();
        } catch (RequestException e) {
            fail("testHostTraces failed=" + e);
        }
    }

    /**
     * Test with a short program name.
     */
    public void testShortProgram() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFAC");
            map.put(Constants.CICS_CHANNEL_KEY, "LSFAC-CHANNEL");
            String[] outContainers = {"RESPONSE-CTN"};
            map.put(Constants.CICS_OUT_CONTAINERS_KEY, outContainers);

            LegStarRequest request = getRequest(map);
            request.getRequestMessage().addDataPart(
                    new ContainerPart("REQUEST-CTN", HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100())));
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            assertEquals("RESPONSE-CTN",
                    request.getResponseMessage().getDataParts().get(0).getPartID());
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
            getConnection().commitUOW();
        } catch (RequestException e) {
            fail("testShortProgram failed=" + e);
        }
    }

    /**
     * Send 2 containers and retrieve 2 containers.
     */
    public void test2ContainersIn2Out() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAC");
            map.put(Constants.CICS_CHANNEL_KEY, "LSFILEAC-CHANNEL");
            String[] outContainers = {"ReplyData", "ReplyStatus"};
            map.put(Constants.CICS_OUT_CONTAINERS_KEY, outContainers);
            
            LegStarRequest request = getRequest(map);
            request.getRequestMessage().addDataPart(new ContainerPart("QueryData",
                    HostData.toByteArray(LsfileacCases.getHostBytesHexQueryData())));
            request.getRequestMessage().addDataPart(new ContainerPart("QueryLimit",
                    HostData.toByteArray(LsfileacCases.getHostBytesHexQueryData())));
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            /* Get the status container first */
            assertEquals("ReplyStatus",
                    request.getResponseMessage().getDataParts().get(1).getPartID());
            assertEquals(LsfileacCases.getHostBytesHexReplyStatus(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(1).getContent()));
            /* Then get the data container */
            assertEquals("ReplyData",
                    request.getResponseMessage().getDataParts().get(0).getPartID());
            assertEquals(LsfileacCases.getHostBytesHexReplyData(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
            getConnection().commitUOW();
        } catch (RequestException e) {
            fail("test2ContainersIn2Out failed=" + e);
        }
    }

    /**
     * Output containers are always optional. If we request the wrong one, there is no errors
     * but the content of the wrong container should be null.
     */
    public void test1ContainersInWithWrongOutContainerName() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAC");
            map.put(Constants.CICS_CHANNEL_KEY, "LSFILEAC-CHANNEL");
            String[] outContainers = {"ReplyBibi", "ReplyStatus"};
            map.put(Constants.CICS_OUT_CONTAINERS_KEY, outContainers);

            LegStarRequest request = getRequest(map);
            request.getRequestMessage().addDataPart(new ContainerPart("QueryData",
                    HostData.toByteArray(LsfileacCases.getHostBytesHexQueryData())));
            request.getRequestMessage().addDataPart(new ContainerPart("QueryLimit",
                    HostData.toByteArray(LsfileacCases.getHostBytesHexQueryData())));
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            /* Get the status container first */
            assertEquals("ReplyStatus",
                    request.getResponseMessage().getDataParts().get(1).getPartID());
            assertEquals(LsfileacCases.getHostBytesHexReplyStatus(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(1).getContent()));
            /* Then get the data container */
            assertEquals("ReplyBibi",
                    request.getResponseMessage().getDataParts().get(0).getPartID());
            assertTrue(null == request.getResponseMessage().getDataParts().get(0).getContent());
            getConnection().commitUOW();
        } catch (RequestException e) {
            fail("test1ContainersInWithWrongOutContainerName failed=" + e);
        }
    }

    /**
     * Test with a single input container and 2 output containers.
     */
    public void test1ContainersIn2Out() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAC");
            map.put(Constants.CICS_CHANNEL_KEY, "LSFILEAC-CHANNEL");
            String[] outContainers = {"ReplyData", "ReplyStatus"};
            map.put(Constants.CICS_OUT_CONTAINERS_KEY, outContainers);
            
            LegStarRequest request = getRequest(map);
            request.getRequestMessage().addDataPart(new ContainerPart("QueryData",
                    HostData.toByteArray(LsfileacCases.getHostBytesHexQueryData())));
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            /* Get the status container first */
            assertEquals("ReplyStatus",
                    request.getResponseMessage().getDataParts().get(1).getPartID());
            assertEquals(LsfileacCases.getHostBytesHexReplyStatus(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(1).getContent()));
            /* Then get the data container */
            assertEquals("ReplyData",
                    request.getResponseMessage().getDataParts().get(0).getPartID());
            assertEquals(LsfileacCases.getHostBytesHexReplyData(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
            getConnection().commitUOW();
        } catch (RequestException e) {
            fail("test1ContainersIn2Out failed=" + e);
        }
    }

    /** 
     * Without any input containers, the host program returns the entire content (new style).
     */
    public void test0ContainersIn2Out() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAC");
            map.put(Constants.CICS_CHANNEL_KEY, "LSFILEAC-CHANNEL");
            String[] outContainers = {"ReplyData", "ReplyStatus"};
            map.put(Constants.CICS_OUT_CONTAINERS_KEY, outContainers);
            
            LegStarRequest request = getRequest(map);
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);

            /* Check */
            assertTrue(request.getResponseMessage() != null);
            assertEquals(2, request.getResponseMessage().getDataParts().size());
            assertEquals(3402, request.getResponseMessage().getDataParts().get(0).getContent().length);
            assertEquals(LsfileacCases.getHostBytesHexReplyStatus(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(1).getContent()));
        } catch (RequestException e) {
            fail("test0ContainersIn2Out failed=" + e);
        }

    }

    /**
     * Test with a large container.
     */
    public void testLargeContainer() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1VOLUMC");
            map.put(Constants.CICS_CHANNEL_KEY, "T1VOLUMC-CHANNEL");
            String[] outContainers = {"RESPONSE-CTN"};
            map.put(Constants.CICS_OUT_CONTAINERS_KEY, outContainers);
            
            LegStarRequest request = getRequest(map);
            request.getRequestMessage().addDataPart(
                    new ContainerPart("REQUEST-CTN", T1volumeCases.getHostBytes(65536)));
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            assertEquals("RESPONSE-CTN",
                    request.getResponseMessage().getDataParts().get(0).getPartID());
            T1volumeCases.checkByteArray(request.getResponseMessage().getDataParts().get(0).getContent());
            getConnection().commitUOW();
        } catch (RequestException e) {
            fail("testLargeContainer failed=" + e);
        }
    }

    /**
     * By iterating thru the execution of the large container test case, we make sure
     * memory if effectively reclaimed between calls and CICS does not SOS.
     */
    public void testForCICSSOS() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1VOLUMC");
            map.put(Constants.CICS_CHANNEL_KEY, "T1VOLUMC-CHANNEL");
            String[] outContainers = {"RESPONSE-CTN"};
            map.put(Constants.CICS_OUT_CONTAINERS_KEY, outContainers);
            
            LegStarMessagePart part = new ContainerPart("REQUEST-CTN", T1volumeCases.getHostBytes(65536));
            for (int i = 0; i < MAX_ITERATIONS; i++) {
                LegStarRequest request = getRequest(map);
                request.getRequestMessage().addDataPart(part);
                getConnection().sendRequest(request);
                getConnection().recvResponse(request);
                assertEquals("RESPONSE-CTN",
                        request.getResponseMessage().getDataParts().get(0).getPartID());
                T1volumeCases.checkByteArray(request.getResponseMessage().getDataParts().get(0).getContent());
                getConnection().commitUOW();
            }
            getConnection().close();
        } catch (RequestException e) {
            fail("testLargeContainer failed=" + e);
        }
    }
}
