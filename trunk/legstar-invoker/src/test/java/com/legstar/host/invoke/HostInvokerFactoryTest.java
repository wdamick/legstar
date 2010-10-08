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
package com.legstar.host.invoke;

import com.legstar.host.invoke.model.HostProgramException;
import com.legstar.messaging.LegStarAddress;

import junit.framework.TestCase;

/**
 * Test HostInvokerFactory.
 *
 */
public class HostInvokerFactoryTest extends TestCase {

    /** Configuration file. */
    private static final String CONFIG_FILE = "config0.xml";

    /**
     * Since the configuration is a singleton, this test will fail if any previous test 
     * successfully loaded a configuration. This is why it is manual.
     *  */
    public void manualtestWrongConfigurationFile() {
        try {
            LegStarAddress address = new LegStarAddress("TheMainframe");
            HostInvokerFactory.createHostInvoker(
                    "tarata.tsointsoin", address, new HostProgramProperties("lsfileae.properties"));
            fail("testWrongConfigurationFile failed ");
        } catch (HostInvokerException e) {
            assertEquals("org.apache.commons.configuration.ConfigurationException:"
                    + " Cannot locate configuration source tarata.tsointsoin", e.getMessage());
        } catch (HostProgramException e) {
            fail(e.toString());
        }
    }

    /**
     * Test with an invalid endpoint.
     */
    public void testWrongEndpoint() {
        try {
            LegStarAddress address = new LegStarAddress("NotAMainframe");
            HostInvokerFactory.createHostInvoker(
                    CONFIG_FILE, address, new HostProgramProperties("lsfileae.properties"));
            fail("testWrongEndpoint failed ");
        } catch (HostInvokerException e) {
            assertEquals("com.legstar.config.LegStarConfigurationException:"
                    + " The requested endpoint:NotAMainframe is not defined.", e.getMessage());
        } catch (HostProgramException e) {
            fail(e.toString());
        }
    }

    /**
     * Test with invalid program properties file.
     */
    public void testWrongProgramAttributesFile() {
        try {
            LegStarAddress address = new LegStarAddress("TheMainframe");
            HostInvokerFactory.createHostInvoker(
                    CONFIG_FILE, address, new HostProgramProperties("goblin.properties"));
            fail("testWrongProgramAttributesFile failed ");
        } catch (HostInvokerException e) {
            fail(e.toString());
        } catch (HostProgramException e) {
            assertEquals("java.io.FileNotFoundException: goblin.properties", e.getMessage());
        }
    }


    /**
     * Test invoke with Containers.
     */
    public void testInstantiateContainerInvoke() {
        try {
            LegStarAddress address = new LegStarAddress("TheMainframe");
            HostInvoker invoker = HostInvokerFactory.createHostInvoker(
                    CONFIG_FILE, address, new HostProgramProperties("container1.properties"));
            assertTrue(invoker instanceof com.legstar.host.invoke.ContainerInvoker);
        } catch (HostInvokerException e) {
            fail("testWrongProgramAttributesFile failed " + e.getMessage());
        } catch (HostProgramException e) {
            fail(e.toString());
        }
    }

    /**
     * Test invoke with Commarea.
     */
    public void testInstantiateCommareaInvoke() {
        try {
            LegStarAddress address = new LegStarAddress("TheMainframe");
            HostInvoker invoker = HostInvokerFactory.createHostInvoker(
                    CONFIG_FILE, address, new HostProgramProperties("lsfileae.properties"));
            assertTrue(invoker instanceof com.legstar.host.invoke.CommareaInvoker);
        } catch (HostInvokerException e) {
            fail("testWrongProgramAttributesFile failed " + e.getMessage());
        } catch (HostProgramException e) {
            fail(e.toString());
        }
    }
}
