/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.host.invoke;

import com.legstar.messaging.Address;

import junit.framework.TestCase;

public class HostInvokerFactoryTest extends TestCase {
	
	private static final String CONFIG_FILE = "config0.xml";

	/* Since the configuration is a singleton, this test will fail if any previous test 
	 * sucessfully loaded a configuration. This is why it is manual. */
	public void ManualtestWrongConfigurationFile() {
		try {
			Address address = new Address("TheMainframe");
			@SuppressWarnings("unused")
			HostInvoker invoker = HostInvokerFactory.createHostInvoker("tarata.tsointsoin", address, "lsfileae.properties");
			fail("testWrongConfigurationFile failed ");
		} catch (HostInvokerException e) {
			assertEquals("org.apache.commons.configuration.ConfigurationException: Cannot locate configuration source tarata.tsointsoin", e.getMessage());
		}
	}
	public void testWrongEndpoint() {
		try {
			Address address = new Address("NotAMainframe");
			@SuppressWarnings("unused")
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "lsfileae.properties");
			fail("testWrongEndpoint failed ");
		} catch (HostInvokerException e) {
			assertEquals("org.apache.commons.configuration.ConfigurationException: The requested endpoint:NotAMainframe is not defined.", e.getMessage());
		}
	}
	
	public void testWrongProgramAttributesFile() {
		try {
			Address address = new Address("TheMainframe");
			@SuppressWarnings("unused")
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "goblin.properties");
			fail("testWrongProgramAttributesFile failed ");
		} catch (HostInvokerException e) {
			assertEquals("java.io.FileNotFoundException: goblin.properties", e.getMessage());
		}
	}


	public void testInstanciateContainerInvoke() {
		try {
			Address address = new Address("TheMainframe");
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");
			assertTrue(invoker instanceof com.legstar.host.invoke.ContainerInvoker );
		} catch (HostInvokerException e) {
			fail("testWrongProgramAttributesFile failed " + e.getMessage());		}
	}
	public void testInstanciateCommareaInvoke() {
		try {
			Address address = new Address("TheMainframe");
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "lsfileae.properties");
			assertTrue(invoker instanceof com.legstar.host.invoke.CommareaInvoker );
		} catch (HostInvokerException e) {
			fail("testWrongProgramAttributesFile failed " + e.getMessage());		}
	}
}
