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

import com.legstar.coxb.host.HostData;
import com.legstar.host.AbstractTester;
import com.legstar.messaging.LegStarAddress;
import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.test.coxb.VararcomCases;

/**
 * Test CommareaInvoker.
 */
public class CommareaInvokerTest extends AbstractTester {

    /** Test a successful access to LSFILEAE. */
    public void testValidInvokeCommarea()  {
        try {
            LegStarAddress address = new LegStarAddress("TheMainframe");
            address.setHostUserID(HOST_USERID);
            address.setHostPassword(HOST_PASSWORD);
            HostInvoker invoker = HostInvokerFactory.createHostInvoker(
                    CONFIG_FILE, address, "lsfileae.properties");
            byte[] responseBytes = invoker.invoke(getName(),
                    HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100()));
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(responseBytes));

        } catch (HostInvokerException e) {
            fail("testValidInvoke failed " + e);
        }
    }

    /** Test failure when program properties are wrong.*/
    public void testWrongProgInvokeCommarea() {
        try {
            LegStarAddress address = new LegStarAddress("TheMainframe");
            address.setHostUserID(HOST_USERID);
            address.setHostPassword(HOST_PASSWORD);
            HostInvoker invoker = HostInvokerFactory.createHostInvoker(
                    CONFIG_FILE, address, "wrongprog.properties");
            invoker.invoke(getName(),
                    HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100()));
            fail("testWrongProgInvokeCommarea failed ");

        } catch (HostInvokerException e) {
            assertTrue(e.getMessage().contains("com.legstar.host.access.HostAccessStrategyException:"
                    + " com.legstar.messaging.RequestException:"
                    + " CICS command=LINK COMMAREA failed, resp=PGMIDERR, resp2="));
        }
    }

    /** When passed an empty address, the parameters should come from the base configuration. */
    public void testEmptyAddressWithBinding() {
        try {
            HostInvoker invoker = HostInvokerFactory.createHostInvoker(
                    CONFIG_FILE, null, "lsfileae.properties");
            byte[] responseBytes = invoker.invoke(getName(),
                    HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100()));
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(responseBytes));

        } catch (HostInvokerException e) {
            fail("testEmptyAddress failed " + e);
        }
    }

    /** When address is partially filled, the rest should come from the base configuration. */
    public void testPartiallyEmptyAddress() {
        try {
            LegStarAddress address = new LegStarAddress("");
            address.setHostUserID("IBMUSER");
            address.setHostPassword(HOST_PASSWORD);
            HostInvoker invoker = HostInvokerFactory.createHostInvoker(
                    CONFIG_FILE, address, "lsfileae.properties");
            byte[] responseBytes = invoker.invoke(getName(),
                    HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100()));
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(responseBytes));

        } catch (HostInvokerException e) {
            fail("testPartiallyEmptyAddress failed " + e);
        }
    }

    /** The base configuration uses HTTP connectivity. */
    public void testValidInvokeCommareaOverHttp() {
        try {
            LegStarAddress address = new LegStarAddress("TheMainframe");
            address.setHostUserID(HOST_USERID);
            address.setHostPassword(HOST_PASSWORD);
            HostInvoker invoker = HostInvokerFactory.createHostInvoker(
                    "config4.xml", address, "lsfileae.properties");
            byte[] responseBytes = invoker.invoke(getName(),
                    HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100()));
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(responseBytes));

        } catch (HostInvokerException e) {
            fail("testValidInvokeCommareaOverHttp failed " + e);
        }
    }

    /** Test a successful access to VARARCOM. */
    public void testInvokeVararcom()  {
        try {
            LegStarAddress address = new LegStarAddress("TheMainframe");
            address.setHostUserID(HOST_USERID);
            address.setHostPassword(HOST_PASSWORD);
            HostInvoker invoker = HostInvokerFactory.createHostInvoker(
                    CONFIG_FILE, address, "vararcom.properties");
            byte[] responseBytes = invoker.invoke(getName(),
                    HostData.toByteArray(VararcomCases.getHostBytesHexEmpty()));
            assertEquals(VararcomCases.getHostBytesHex36(),
                    HostData.toHexString(responseBytes));

        } catch (HostInvokerException e) {
            fail("testValidInvoke failed " + e);
        }
    }

}
