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
package com.legstar.host.invoke;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;
import com.legstar.host.AbstractTester;
import com.legstar.messaging.LegStarAddress;
import com.legstar.test.coxb.LsfileacCases;
import com.legstar.test.coxb.lsfileac.QueryData;
import com.legstar.test.coxb.lsfileac.QueryLimit;
import com.legstar.test.coxb.lsfileac.bind.QueryDataBinding;
import com.legstar.test.coxb.lsfileac.bind.QueryLimitBinding;
import com.legstar.test.coxb.lsfileac.bind.ReplyDataBinding;
import com.legstar.test.coxb.lsfileac.bind.ReplyStatusBinding;

/**
 * Test ContainerInvoker.
 */
public class ContainerInvokerTest extends AbstractTester {

    /** 
     * Check that the factory correctly returns a containerInvoker. 
     * @throws HostInvokerException if test fails
     */
    public void testContainerInvoker() throws HostInvokerException {
        LegStarAddress address = new LegStarAddress("CICSTS31");
        HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");
        assertTrue(invoker instanceof com.legstar.host.invoke.ContainerInvoker);
    }

    /** The container invoker should not accept a call to invoke without non-map parameters. */
    public void testInvokeWrongMethod() {
        try {
            LegStarAddress address = new LegStarAddress("CICSTS31");
            HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");
            ICobolComplexBinding ccbin = null;
            ICobolComplexBinding ccbout = null;
            invoker.invoke("testInvokeWrongMethod", ccbin, ccbout);
            fail("Method should not be supported");
        } catch (HostInvokerException e) {
            assertEquals("Unsupported method for CICS containers", e.getMessage());
        }
        try {
            LegStarAddress address = new LegStarAddress("CICSTS31");
            HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");
            invoker.invoke("testInvokeWrongMethod", new byte[0]);
            fail("Method should not be supported");
        } catch (HostInvokerException e) {
            assertEquals("Unsupported method for CICS containers", e.getMessage());
        }
    }

    /** 
     * Test with 2 input containers and 2 output containers (old style). 
     * @throws HostInvokerException if invoke fails
     * @throws HostException if host data is invalid
     */
    public void test2ContainersIn2OutWithBinding() throws HostInvokerException, HostException {
        LegStarAddress address = new LegStarAddress("CICSTS31");
        HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");

        /* There are 2 containers for request */
        QueryData queryData = LsfileacCases.getJavaObjectQueryData();
        QueryLimit queryLimit = LsfileacCases.getJavaObjectQueryLimit();

        /* Decorate object trees for static binding */
        QueryDataBinding queryDataBin = new QueryDataBinding(queryData);
        QueryLimitBinding queryLimitBin = new QueryLimitBinding(queryLimit);

        /* Prepare output objects */
        ReplyDataBinding replyDataBin = new ReplyDataBinding();
        ReplyStatusBinding replyStatusBin = new ReplyStatusBinding();

        /* Map containers with corresponding object trees */
        Map < String, ICobolComplexBinding > inParts =
            new LinkedHashMap < String, ICobolComplexBinding >(); 
        inParts.put("QueryData", queryDataBin);
        inParts.put("QueryLimit", queryLimitBin);

        Map < String, ICobolComplexBinding > outParts =
            new LinkedHashMap < String, ICobolComplexBinding >(); 
        outParts.put("ReplyData", replyDataBin);
        outParts.put("ReplyStatus", replyStatusBin);

        /* call */
        invoker.invoke("test2ContainersIn2Out", inParts, outParts);

        /* Check */
        LsfileacCases.checkJavaObjectReplyStatus(replyStatusBin.getReplyStatus());
        LsfileacCases.checkJavaObjectReplyData(replyDataBin.getReplyData());

    }

    /** 
     * Test with 2 input containers and 2 output containers (new style).
     * @throws HostInvokerException if invoke fails
     * @throws HostException if host data is invalid
     */
    public void test2ContainersIn2Out() throws HostInvokerException, HostException {
        LegStarAddress address = new LegStarAddress("CICSTS31");
        HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");

        /* Get raw mainframe bytes */
        byte[] queryDataBin = HostData.toByteArray(LsfileacCases.getHostBytesHexQueryData());
        byte[] queryLimitBin = HostData.toByteArray(LsfileacCases.getHostBytesHexQueryLimit());

        /* Map containers with corresponding byte arrays */
        Map < String, byte[] > inParts = new LinkedHashMap < String, byte[] >(); 
        inParts.put("QueryData", queryDataBin);
        inParts.put("QueryLimit", queryLimitBin);

        /* call */
        Map < String, byte[] > outParts = invoker.invoke("test2ContainersIn2Out", inParts);

        /* Check */
        assertTrue(outParts != null);
        assertEquals(2, outParts.size());
        assertEquals(LsfileacCases.getHostBytesHexReplyData(),
                HostData.toHexString(outParts.get("ReplyData")));
        assertEquals(LsfileacCases.getHostBytesHexReplyStatus(),
                HostData.toHexString(outParts.get("ReplyStatus")));

    }

    /** 
     * Without any input containers, the host program returns the entire content (old style).
     * @throws HostInvokerException if invoke fails
     * @throws HostException if host data is invalid
     */
    public void test0ContainersIn2OutWithBinding() throws HostInvokerException, HostException {
        LegStarAddress address = new LegStarAddress("CICSTS31");
        HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");

        /* Prepare output objects */
        ReplyDataBinding replyDataBin = new ReplyDataBinding();
        ReplyStatusBinding replyStatusBin = new ReplyStatusBinding();

        /* Map containers with corresponding object trees */
        Map < String, ICobolComplexBinding > inParts =
            new LinkedHashMap < String, ICobolComplexBinding >(); 

        Map < String, ICobolComplexBinding > outParts =
            new LinkedHashMap < String, ICobolComplexBinding >(); 
        outParts.put("ReplyData", replyDataBin);
        outParts.put("ReplyStatus", replyStatusBin);

        /* call */
        invoker.invoke("test0ContainersIn2Out", inParts, outParts);

        /* Check */
        LsfileacCases.checkJavaObjectReplyStatus(replyStatusBin.getReplyStatus());
        LsfileacCases.checkJavaObjectReplyDataFull(replyDataBin.getReplyData());
    }

    /** 
     * Without any input containers, the host program returns the entire content (new style).
     * @throws HostInvokerException if invoke fails
     * @throws HostException if host data is invalid
     */
    public void test0ContainersIn2Out() throws HostInvokerException, HostException {
        LegStarAddress address = new LegStarAddress("CICSTS31");
        HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");

        /* Map containers with corresponding byte arrays */
        Map < String, byte[] > inParts = new LinkedHashMap < String, byte[] >(); 

        /* call */
        Map < String, byte[] > outParts = invoker.invoke("test2ContainersIn2Out", inParts);

        /* Check */
        assertTrue(outParts != null);
        assertEquals(2, outParts.size());
        assertEquals(3481, outParts.get("ReplyData").length);
        assertEquals(LsfileacCases.getHostBytesHexReplyStatus(),
                HostData.toHexString(outParts.get("ReplyStatus")));

    }

    /** 
     * When nothing is selected on the host, there is no data container back (old style).
     * @throws HostInvokerException if invoke fails
     * @throws HostException if host data is invalid
     */
    public void test2ContainersIn1OutWithBinding() throws HostInvokerException, HostException {
        LegStarAddress address = new LegStarAddress("CICSTS31");
        HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");

        /* There are 2 containers for request */
        QueryData queryData = LsfileacCases.getJavaObjectQueryDataNoMatch();
        QueryLimit queryLimit = LsfileacCases.getJavaObjectQueryLimit();

        /* Decorate object trees for static binding */
        QueryDataBinding queryDataBin = new QueryDataBinding(queryData);
        QueryLimitBinding queryLimitBin = new QueryLimitBinding(queryLimit);

        /* Prepare output objects */
        ReplyDataBinding replyDataBin = new ReplyDataBinding();
        ReplyStatusBinding replyStatusBin = new ReplyStatusBinding();

        /* Map containers with corresponding object trees */
        Map < String, ICobolComplexBinding > inParts =
            new LinkedHashMap < String, ICobolComplexBinding >(); 
        inParts.put("QueryData", queryDataBin);
        inParts.put("QueryLimit", queryLimitBin);

        Map < String, ICobolComplexBinding > outParts =
            new LinkedHashMap < String, ICobolComplexBinding >(); 
        outParts.put("ReplyData", replyDataBin);
        outParts.put("ReplyStatus", replyStatusBin);

        /* call */
        invoker.invoke("test2ContainersIn1Out", inParts, outParts);

        /* Check */
        LsfileacCases.checkJavaObjectReplyStatusNoMatch(replyStatusBin.getReplyStatus());
        assertTrue(null == replyDataBin.getReplyData());

    }

    /** 
     * When nothing is selected on the host, there is no data container back (new style).
     * @throws HostInvokerException if invoke fails
     * @throws HostException if host data is invalid
     */
    public void test2ContainersIn1Out() throws HostInvokerException, HostException {
        LegStarAddress address = new LegStarAddress("CICSTS31");
        HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");

        /* Get raw mainframe bytes */
        byte[] queryDataBin = HostData.toByteArray(LsfileacCases.getHostBytesHexQueryDataNoMatch());
        byte[] queryLimitBin = HostData.toByteArray(LsfileacCases.getHostBytesHexQueryLimit());

        /* Map containers with corresponding byte arrays */
        Map < String, byte[] > inParts = new LinkedHashMap < String, byte[] >(); 
        inParts.put("QueryData", queryDataBin);
        inParts.put("QueryLimit", queryLimitBin);

        /* call */
        Map < String, byte[] > outParts = invoker.invoke("test2ContainersIn2Out", inParts);

        /* Check */
        assertTrue(outParts != null);
        assertEquals(2, outParts.size());
        assertEquals(null, outParts.get("ReplyData"));
        assertEquals(LsfileacCases.getHostBytesHexReplyStatusNoMatch(),
                HostData.toHexString(outParts.get("ReplyStatus")));

    }

}
