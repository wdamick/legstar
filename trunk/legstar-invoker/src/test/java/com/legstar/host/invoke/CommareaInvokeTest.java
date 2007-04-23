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

import com.legstar.host.invoke.CommareaInvoke;
import com.legstar.host.invoke.HostInvokeException;
import com.legstar.messaging.Address;
import com.legstar.test.lsfileae.DfhcommareaType;
import com.legstar.test.lsfileae.bind.DfhcommareaTypeBinding;

import junit.framework.TestCase;

public class CommareaInvokeTest extends TestCase {
	
	private static final String CONFIG_FILE = "config0.xml";
	private static final String HOST_USERID = "P390";
	private static final String HOST_PASSWORD = "STREAM2";

	public void testWrongConfigurationFile() {
		try {
			Address address = new Address("TheMainframe");
			address.setHostUserID(HOST_USERID);
			address.setHostPassword(HOST_PASSWORD);
			@SuppressWarnings("unused")
			CommareaInvoke invoker = new CommareaInvoke("tarata.tsointsoin", address, "lsfileae.properties");
			fail("testWrongConfigurationFile failed ");
		} catch (HostInvokeException e) {
			assertEquals("org.apache.commons.configuration.ConfigurationException: Cannot locate configuration source tarata.tsointsoin", e.getMessage());
		}
	}
	
	public void testWrongEndpoint() {
		try {
			Address address = new Address("NotAMainframe");
			address.setHostUserID(HOST_USERID);
			address.setHostPassword(HOST_PASSWORD);
			@SuppressWarnings("unused")
			CommareaInvoke invoker = new CommareaInvoke(CONFIG_FILE, address, "lsfileae.properties");
			fail("testWrongEndpoint failed ");
		} catch (HostInvokeException e) {
			assertEquals("org.apache.commons.configuration.ConfigurationException: The requested endpoint:NotAMainframe is not defined.", e.getMessage());
		}
	}
	
	public void testValidInstanciation() {
		try {
			Address address = new Address("TheMainframe");
			address.setHostUserID(HOST_USERID);
			address.setHostPassword(HOST_PASSWORD);
			CommareaInvoke invoker = new CommareaInvoke(CONFIG_FILE, address, "lsfileae.properties");
			assertTrue(invoker != null);
		} catch (HostInvokeException e) {
			fail("testValidInstanciation failed " + e);
		}
	}

	public void testValidInvokeCommarea() {
		try {
			Address address = new Address("TheMainframe");
			address.setHostUserID(HOST_USERID);
			address.setHostPassword(HOST_PASSWORD);
			CommareaInvoke invoker = new CommareaInvoke(CONFIG_FILE, address, "lsfileae.properties");
		    /* The JAXB input factory. */
		    com.legstar.test.lsfileae.ObjectFactory jaxbInFactory =
		          new com.legstar.test.lsfileae.ObjectFactory(); 
		    
		    /* The JAXB output factory. */
		    com.legstar.test.lsfileae.ObjectFactory jaxbOutFactory =
		          new com.legstar.test.lsfileae.ObjectFactory(); 
		    
		    /* The request java object tree */
		    DfhcommareaType request	= jaxbInFactory.createDfhcommareaType();
		    request.setComNumber(100L);
		    
		    /* Decorate object tree for static binding */
		    DfhcommareaTypeBinding ccbin = new DfhcommareaTypeBinding(
		    		jaxbInFactory, request);
		    
		    /* Prepare output object */
		    DfhcommareaTypeBinding ccbout =
		          new DfhcommareaTypeBinding(jaxbOutFactory);
		    
		    /* call */
		    invoker.invoke("MyNewRequest", ccbin, ccbout);
		    
		    /* Check */
		    assertEquals(100, ccbout.getJaxbObject().getComNumber());
		    assertEquals("$0100.11", ccbout.getJaxbObject().getComAmount());
		    assertEquals("*********", ccbout.getJaxbObject().getComComment());
		    assertEquals("26 11 81", ccbout.getJaxbObject().getComDate());
		    assertEquals("SURREY, ENGLAND     ", ccbout.getJaxbObject().getComPersonal().getComAddress());
		    assertEquals("S. D. BORMAN        ", ccbout.getJaxbObject().getComPersonal().getComName());
		    assertEquals("32156778", ccbout.getJaxbObject().getComPersonal().getComPhone());
		} catch (HostInvokeException e) {
			fail("testValidInvoke failed " + e);
		}
	}
	
	public void testWrongProgInvokeCommarea() {
		try {
			Address address = new Address("TheMainframe");
			address.setHostUserID(HOST_USERID);
			address.setHostPassword(HOST_PASSWORD);
			CommareaInvoke invoker = new CommareaInvoke(CONFIG_FILE, address, "wrongprog.properties");
		    /* The JAXB input factory. */
		    com.legstar.test.lsfileae.ObjectFactory jaxbInFactory =
		          new com.legstar.test.lsfileae.ObjectFactory(); 
		    
		    /* The JAXB output factory. */
		    com.legstar.test.lsfileae.ObjectFactory jaxbOutFactory =
		          new com.legstar.test.lsfileae.ObjectFactory(); 
		    
		    /* The request java object tree */
		    DfhcommareaType request	= jaxbInFactory.createDfhcommareaType();
		    request.setComNumber(100L);
		    
		    /* Decorate object tree for static binding */
		    DfhcommareaTypeBinding ccbin = new DfhcommareaTypeBinding(
		    		jaxbInFactory, request);
		    
		    /* Prepare output object */
		    DfhcommareaTypeBinding ccbout =
		          new DfhcommareaTypeBinding(jaxbOutFactory);
		    
		    /* call */
		    invoker.invoke("MyNewRequest", ccbin, ccbout);
			fail("testWrongProgInvokeCommarea failed ");
		    /* Check */
		} catch (HostInvokeException e) {
			assertEquals("com.legstar.host.access.HostAccessStrategyException: com.legstar.messaging.RequestException: CICS command=LINK failed, resp=PGMIDERR, resp2=3", e.getMessage());
		}
	}
	
	public void testEmptyAddress() {
		try {
			CommareaInvoke invoker = new CommareaInvoke(CONFIG_FILE, null, "lsfileae.properties");
			assertEquals("TheMainframe", invoker.getAddress().getEndPointName());
			assertEquals("IBM01140", invoker.getAddress().getHostCharset());
			assertEquals("STREAM2", invoker.getAddress().getHostPassword());
			assertEquals("P390", invoker.getAddress().getHostUserID());
			assertEquals(true, invoker.getAddress().isHostTraceMode());
		    /* The JAXB input factory. */
		    com.legstar.test.lsfileae.ObjectFactory jaxbInFactory =
		          new com.legstar.test.lsfileae.ObjectFactory(); 
		    
		    /* The JAXB output factory. */
		    com.legstar.test.lsfileae.ObjectFactory jaxbOutFactory =
		          new com.legstar.test.lsfileae.ObjectFactory(); 
		    
		    /* The request java object tree */
		    DfhcommareaType request	= jaxbInFactory.createDfhcommareaType();
		    request.setComNumber(100L);
		    
		    /* Decorate object tree for static binding */
		    DfhcommareaTypeBinding ccbin = new DfhcommareaTypeBinding(
		    		jaxbInFactory, request);
		    
		    /* Prepare output object */
		    DfhcommareaTypeBinding ccbout =
		          new DfhcommareaTypeBinding(jaxbOutFactory);
		    
		    /* call */
		    invoker.invoke("MyNewRequest", ccbin, ccbout);
		    
		    /* Check */
		    assertEquals(100, ccbout.getJaxbObject().getComNumber());
		    assertEquals("$0100.11", ccbout.getJaxbObject().getComAmount());
		    assertEquals("*********", ccbout.getJaxbObject().getComComment());
		    assertEquals("26 11 81", ccbout.getJaxbObject().getComDate());
		    assertEquals("SURREY, ENGLAND     ", ccbout.getJaxbObject().getComPersonal().getComAddress());
		    assertEquals("S. D. BORMAN        ", ccbout.getJaxbObject().getComPersonal().getComName());
		    assertEquals("32156778", ccbout.getJaxbObject().getComPersonal().getComPhone());
		} catch (HostInvokeException e) {
			fail("testEmptyAddress failed " + e);
		}
	}

	public void testPartiallyEmptyAddress() {
		try {
			Address address = new Address("");
			address.setHostUserID("IBMUSER");
			address.setHostPassword(HOST_PASSWORD);
			CommareaInvoke invoker = new CommareaInvoke(CONFIG_FILE, address, "lsfileae.properties");
			assertEquals("TheMainframe", invoker.getAddress().getEndPointName());
			assertEquals("IBM01140", invoker.getAddress().getHostCharset());
			assertEquals("STREAM2", invoker.getAddress().getHostPassword());
			assertEquals("IBMUSER", invoker.getAddress().getHostUserID());
			assertEquals(true, invoker.getAddress().isHostTraceMode());
		    /* The JAXB input factory. */
		    com.legstar.test.lsfileae.ObjectFactory jaxbInFactory =
		          new com.legstar.test.lsfileae.ObjectFactory(); 
		    
		    /* The JAXB output factory. */
		    com.legstar.test.lsfileae.ObjectFactory jaxbOutFactory =
		          new com.legstar.test.lsfileae.ObjectFactory(); 
		    
		    /* The request java object tree */
		    DfhcommareaType request	= jaxbInFactory.createDfhcommareaType();
		    request.setComNumber(100L);
		    
		    /* Decorate object tree for static binding */
		    DfhcommareaTypeBinding ccbin = new DfhcommareaTypeBinding(
		    		jaxbInFactory, request);
		    
		    /* Prepare output object */
		    DfhcommareaTypeBinding ccbout =
		          new DfhcommareaTypeBinding(jaxbOutFactory);
		    
		    /* call */
		    invoker.invoke("MyNewRequest", ccbin, ccbout);
		    
		    /* Check */
		    assertEquals(100, ccbout.getJaxbObject().getComNumber());
		    assertEquals("$0100.11", ccbout.getJaxbObject().getComAmount());
		    assertEquals("*********", ccbout.getJaxbObject().getComComment());
		    assertEquals("26 11 81", ccbout.getJaxbObject().getComDate());
		    assertEquals("SURREY, ENGLAND     ", ccbout.getJaxbObject().getComPersonal().getComAddress());
		    assertEquals("S. D. BORMAN        ", ccbout.getJaxbObject().getComPersonal().getComName());
		    assertEquals("32156778", ccbout.getJaxbObject().getComPersonal().getComPhone());
		} catch (HostInvokeException e) {
			fail("testEmptyAddress failed " + e);
		}
	}
	public void testValidInvokeCommareaOverHttp() {
		try {
			Address address = new Address("TheMainframe");
			address.setHostUserID(HOST_USERID);
			address.setHostPassword(HOST_PASSWORD);
			CommareaInvoke invoker = new CommareaInvoke("config4.xml", address, "lsfileae.properties");
		    /* The JAXB input factory. */
		    com.legstar.test.lsfileae.ObjectFactory jaxbInFactory =
		          new com.legstar.test.lsfileae.ObjectFactory(); 
		    
		    /* The JAXB output factory. */
		    com.legstar.test.lsfileae.ObjectFactory jaxbOutFactory =
		          new com.legstar.test.lsfileae.ObjectFactory(); 
		    
		    /* The request java object tree */
		    DfhcommareaType request	= jaxbInFactory.createDfhcommareaType();
		    request.setComNumber(100L);
		    
		    /* Decorate object tree for static binding */
		    DfhcommareaTypeBinding ccbin = new DfhcommareaTypeBinding(
		    		jaxbInFactory, request);
		    
		    /* Prepare output object */
		    DfhcommareaTypeBinding ccbout =
		          new DfhcommareaTypeBinding(jaxbOutFactory);
		    
		    /* call */
		    invoker.invoke("MyNewRequest", ccbin, ccbout);
		    
		    /* Check */
		    assertEquals(100, ccbout.getJaxbObject().getComNumber());
		    assertEquals("$0100.11", ccbout.getJaxbObject().getComAmount());
		    assertEquals("*********", ccbout.getJaxbObject().getComComment());
		    assertEquals("26 11 81", ccbout.getJaxbObject().getComDate());
		    assertEquals("SURREY, ENGLAND     ", ccbout.getJaxbObject().getComPersonal().getComAddress());
		    assertEquals("S. D. BORMAN        ", ccbout.getJaxbObject().getComPersonal().getComName());
		    assertEquals("32156778", ccbout.getJaxbObject().getComPersonal().getComPhone());
		} catch (HostInvokeException e) {
			fail("testValidInvoke failed " + e);
		}
	}
	
}
