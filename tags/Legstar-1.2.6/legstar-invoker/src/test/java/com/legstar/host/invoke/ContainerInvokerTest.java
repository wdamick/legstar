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
package com.legstar.host.invoke;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.coxb.host.HostData;
import com.legstar.host.AbstractTester;
import com.legstar.host.invoke.model.HostProgramException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.test.coxb.LsfileacCases;

/**
 * Test ContainerInvoker.
 */
public class ContainerInvokerTest extends AbstractTester {

    /** 
     * Check that the factory correctly returns a containerInvoker. 
     * @throws Exception if test fails
     */
    public void testContainerInvoker() throws Exception {
        LegStarAddress address = new LegStarAddress("CICSTS31");
        HostInvoker invoker = HostInvokerFactory.createHostInvoker(
                CONFIG_FILE, address, new HostProgramProperties("container1.properties"));
        assertTrue(invoker instanceof com.legstar.host.invoke.ContainerInvoker);
    }

    /** The container invoker should not accept a call to invoke without non-map parameters. */
    public void testInvokeWrongMethod() {
        try {
            LegStarAddress address = new LegStarAddress("CICSTS31");
            HostInvoker invoker = HostInvokerFactory.createHostInvoker(
                    CONFIG_FILE, address, new HostProgramProperties("container1.properties"));
            invoker.invoke(getName(), new byte[0]);
            fail("Method should not be supported");
        } catch (HostInvokerException e) {
            assertEquals("Unsupported method for CICS containers", e.getMessage());
        } catch (HostProgramException e) {
            fail(e.toString());
        }
    }

    /** 
     * Test with 2 input containers and 2 output containers (new style).
     * @throws Exception if host data is invalid
     */
    public void test2ContainersIn2Out() throws Exception {
        LegStarAddress address = new LegStarAddress("CICSTS31");
        HostInvoker invoker = HostInvokerFactory.createHostInvoker(
                CONFIG_FILE, address, new HostProgramProperties("container1.properties"));

        /* Get raw mainframe bytes */
        byte[] queryDataBin = HostData.toByteArray(LsfileacCases.getHostBytesHexQueryData());
        byte[] queryLimitBin = HostData.toByteArray(LsfileacCases.getHostBytesHexQueryLimit());

        /* Map containers with corresponding byte arrays */
        Map < String, byte[] > inParts = new LinkedHashMap < String, byte[] >(); 
        inParts.put("QueryData", queryDataBin);
        inParts.put("QueryLimit", queryLimitBin);

        /* call */
        Map < String, byte[] > outParts = invoker.invoke(getName(), inParts);

        /* Check */
        assertTrue(outParts != null);
        assertEquals(2, outParts.size());
        assertEquals(LsfileacCases.getHostBytesHexReplyData(),
                HostData.toHexString(outParts.get("ReplyData")));
        assertEquals(LsfileacCases.getHostBytesHexReplyStatus(),
                HostData.toHexString(outParts.get("ReplyStatus")));

    }

    /** 
     * When nothing is selected on the host, there is no data container back (new style).
     * @throws Exception if host data is invalid
     */
    public void test2ContainersIn1Out() throws Exception {
        LegStarAddress address = new LegStarAddress("CICSTS31");
        HostInvoker invoker = HostInvokerFactory.createHostInvoker(
                CONFIG_FILE, address, new HostProgramProperties("container1.properties"));

        /* Get raw mainframe bytes */
        byte[] queryDataBin = HostData.toByteArray(LsfileacCases.getHostBytesHexQueryDataNoMatch());
        byte[] queryLimitBin = HostData.toByteArray(LsfileacCases.getHostBytesHexQueryLimit());

        /* Map containers with corresponding byte arrays */
        Map < String, byte[] > inParts = new LinkedHashMap < String, byte[] >(); 
        inParts.put("QueryData", queryDataBin);
        inParts.put("QueryLimit", queryLimitBin);

        /* call */
        Map < String, byte[] > outParts = invoker.invoke(getName(), inParts);

        /* Check */
        assertTrue(outParts != null);
        assertEquals(2, outParts.size());
        assertEquals(null, outParts.get("ReplyData"));
        assertEquals(LsfileacCases.getHostBytesHexReplyStatusNoMatch(),
                HostData.toHexString(outParts.get("ReplyStatus")));

    }

}
