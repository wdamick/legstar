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

import com.legstar.messaging.LegStarAddress;

import junit.framework.TestCase;

public class HostInvokerFactoryTest extends TestCase {
	
	private static final String CONFIG_FILE = "config0.xml";

	/* Since the configuration is a singleton, this test will fail if any previous test 
	 * sucessfully loaded a configuration. This is why it is manual. */
	public void ManualtestWrongConfigurationFile() {
		try {
			LegStarAddress address = new LegStarAddress("TheMainframe");
			@SuppressWarnings("unused")
			HostInvoker invoker = HostInvokerFactory.createHostInvoker("tarata.tsointsoin", address, "lsfileae.properties");
			fail("testWrongConfigurationFile failed ");
		} catch (HostInvokerException e) {
			assertEquals("org.apache.commons.configuration.ConfigurationException: Cannot locate configuration source tarata.tsointsoin", e.getMessage());
		}
	}
	public void testWrongEndpoint() {
		try {
			LegStarAddress address = new LegStarAddress("NotAMainframe");
			@SuppressWarnings("unused")
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "lsfileae.properties");
			fail("testWrongEndpoint failed ");
		} catch (HostInvokerException e) {
			assertEquals("org.apache.commons.configuration.ConfigurationException: The requested endpoint:NotAMainframe is not defined.", e.getMessage());
		}
	}
	
	public void testWrongProgramAttributesFile() {
		try {
			LegStarAddress address = new LegStarAddress("TheMainframe");
			@SuppressWarnings("unused")
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "goblin.properties");
			fail("testWrongProgramAttributesFile failed ");
		} catch (HostInvokerException e) {
			assertEquals("java.io.FileNotFoundException: goblin.properties", e.getMessage());
		}
	}


	public void testInstanciateContainerInvoke() {
		try {
			LegStarAddress address = new LegStarAddress("TheMainframe");
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");
			assertTrue(invoker instanceof com.legstar.host.invoke.ContainerInvoker );
		} catch (HostInvokerException e) {
			fail("testWrongProgramAttributesFile failed " + e.getMessage());		}
	}
	public void testInstanciateCommareaInvoke() {
		try {
			LegStarAddress address = new LegStarAddress("TheMainframe");
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "lsfileae.properties");
			assertTrue(invoker instanceof com.legstar.host.invoke.CommareaInvoker );
		} catch (HostInvokerException e) {
			fail("testWrongProgramAttributesFile failed " + e.getMessage());		}
	}
}
