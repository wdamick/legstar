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

import com.legstar.host.invoke.HostInvokerException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.test.coxb.lsfileae.DfhcommareaType;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaTypeBinding;

import junit.framework.TestCase;

public class CommareaInvokerTest extends TestCase {
	
	private static final String CONFIG_FILE = "config0.xml";
	private static final String HOST_USERID = "P390";
	private static final String HOST_PASSWORD = "STREAM2";

	public void testValidInvokeCommarea()  {
		try {
			LegStarAddress address = new LegStarAddress("TheMainframe");
			address.setHostUserID(HOST_USERID);
			address.setHostPassword(HOST_PASSWORD);
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "lsfileae.properties");
		    /* The JAXB input factory. */
		    com.legstar.test.coxb.lsfileae.ObjectFactory jaxbInFactory =
		          new com.legstar.test.coxb.lsfileae.ObjectFactory(); 
		    
		    /* The request java object tree */
		    DfhcommareaType request	= jaxbInFactory.createDfhcommareaType();
		    request.setComNumber(100L);
		    
		    /* Decorate object tree for static binding */
		    DfhcommareaTypeBinding ccbin = new DfhcommareaTypeBinding(request);
		    
		    /* Prepare output object */
		    DfhcommareaTypeBinding ccbout =
		          new DfhcommareaTypeBinding();
		    
		    /* call */
		    invoker.invoke("MyNewRequest", ccbin, ccbout);
		    
		    /* Check */
		    assertEquals(100, ccbout.getDfhcommareaType().getComNumber());
		    assertEquals("$0100.11", ccbout.getDfhcommareaType().getComAmount());
		    assertEquals("*********", ccbout.getDfhcommareaType().getComComment());
		    assertEquals("26 11 81", ccbout.getDfhcommareaType().getComDate());
		    assertEquals("SURREY, ENGLAND", ccbout.getDfhcommareaType().getComPersonal().getComAddress());
		    assertEquals("S. D. BORMAN", ccbout.getDfhcommareaType().getComPersonal().getComName());
		    assertEquals("32156778", ccbout.getDfhcommareaType().getComPersonal().getComPhone());
		} catch (HostInvokerException e) {
			fail("testValidInvoke failed " + e);
		}
	}
	
	public void testWrongProgInvokeCommarea() {
		try {
			LegStarAddress address = new LegStarAddress("TheMainframe");
			address.setHostUserID(HOST_USERID);
			address.setHostPassword(HOST_PASSWORD);
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "wrongprog.properties");
		    /* The JAXB input factory. */
		    com.legstar.test.coxb.lsfileae.ObjectFactory jaxbInFactory =
		          new com.legstar.test.coxb.lsfileae.ObjectFactory(); 
		    
		    /* The request java object tree */
		    DfhcommareaType request	= jaxbInFactory.createDfhcommareaType();
		    request.setComNumber(100L);
		    
		    /* Decorate object tree for static binding */
		    DfhcommareaTypeBinding ccbin = new DfhcommareaTypeBinding(request);
		    
		    /* Prepare output object */
		    DfhcommareaTypeBinding ccbout =
		          new DfhcommareaTypeBinding();
		    
		    /* call */
		    invoker.invoke("MyNewRequest", ccbin, ccbout);
			fail("testWrongProgInvokeCommarea failed ");
		    /* Check */
		} catch (HostInvokerException e) {
			assertEquals("com.legstar.host.access.HostAccessStrategyException: com.legstar.messaging.RequestException: CICS command=LINK COMMAREA failed, resp=PGMIDERR, resp2=3", e.getMessage());
		}
	}
	
	public void testEmptyAddress() {
		try {
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, null, "lsfileae.properties");
		    /* The JAXB input factory. */
		    com.legstar.test.coxb.lsfileae.ObjectFactory jaxbInFactory =
		          new com.legstar.test.coxb.lsfileae.ObjectFactory(); 
		    
		    /* The request java object tree */
		    DfhcommareaType request	= jaxbInFactory.createDfhcommareaType();
		    request.setComNumber(100L);
		    
		    /* Decorate object tree for static binding */
		    DfhcommareaTypeBinding ccbin = new DfhcommareaTypeBinding(request);
		    
		    /* Prepare output object */
		    DfhcommareaTypeBinding ccbout =
		          new DfhcommareaTypeBinding();
		    
		    /* call */
		    invoker.invoke("MyNewRequest", ccbin, ccbout);
		    
		    /* Check */
		    assertEquals(100, ccbout.getDfhcommareaType().getComNumber());
		    assertEquals("$0100.11", ccbout.getDfhcommareaType().getComAmount());
		    assertEquals("*********", ccbout.getDfhcommareaType().getComComment());
		    assertEquals("26 11 81", ccbout.getDfhcommareaType().getComDate());
		    assertEquals("SURREY, ENGLAND", ccbout.getDfhcommareaType().getComPersonal().getComAddress());
		    assertEquals("S. D. BORMAN", ccbout.getDfhcommareaType().getComPersonal().getComName());
		    assertEquals("32156778", ccbout.getDfhcommareaType().getComPersonal().getComPhone());
		} catch (HostInvokerException e) {
			fail("testEmptyAddress failed " + e);
		}
	}

	public void testPartiallyEmptyAddress() {
		try {
			LegStarAddress address = new LegStarAddress("");
			address.setHostUserID("IBMUSER");
			address.setHostPassword(HOST_PASSWORD);
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "lsfileae.properties");
		    /* The JAXB input factory. */
		    com.legstar.test.coxb.lsfileae.ObjectFactory jaxbInFactory =
		          new com.legstar.test.coxb.lsfileae.ObjectFactory(); 
		    
		    /* The request java object tree */
		    DfhcommareaType request	= jaxbInFactory.createDfhcommareaType();
		    request.setComNumber(100L);
		    
		    /* Decorate object tree for static binding */
		    DfhcommareaTypeBinding ccbin = new DfhcommareaTypeBinding(request);
		    
		    /* Prepare output object */
		    DfhcommareaTypeBinding ccbout =
		          new DfhcommareaTypeBinding();
		    
		    /* call */
		    invoker.invoke("MyNewRequest", ccbin, ccbout);
		    
		    /* Check */
		    assertEquals(100, ccbout.getDfhcommareaType().getComNumber());
		    assertEquals("$0100.11", ccbout.getDfhcommareaType().getComAmount());
		    assertEquals("*********", ccbout.getDfhcommareaType().getComComment());
		    assertEquals("26 11 81", ccbout.getDfhcommareaType().getComDate());
		    assertEquals("SURREY, ENGLAND", ccbout.getDfhcommareaType().getComPersonal().getComAddress());
		    assertEquals("S. D. BORMAN", ccbout.getDfhcommareaType().getComPersonal().getComName());
		    assertEquals("32156778", ccbout.getDfhcommareaType().getComPersonal().getComPhone());
		} catch (HostInvokerException e) {
			fail("testPartiallyEmptyAddress failed " + e);
		}
	}
	public void testValidInvokeCommareaOverHttp() {
		try {
			LegStarAddress address = new LegStarAddress("TheMainframe");
			address.setHostUserID(HOST_USERID);
			address.setHostPassword(HOST_PASSWORD);
			HostInvoker invoker = HostInvokerFactory.createHostInvoker("config4.xml", address, "lsfileae.properties");
		    /* The JAXB input factory. */
		    com.legstar.test.coxb.lsfileae.ObjectFactory jaxbInFactory =
		          new com.legstar.test.coxb.lsfileae.ObjectFactory(); 
		    
		    /* The request java object tree */
		    DfhcommareaType request	= jaxbInFactory.createDfhcommareaType();
		    request.setComNumber(100L);
		    
		    /* Decorate object tree for static binding */
		    DfhcommareaTypeBinding ccbin = new DfhcommareaTypeBinding(request);
		    
		    /* Prepare output object */
		    DfhcommareaTypeBinding ccbout =
		          new DfhcommareaTypeBinding();
		    
		    /* call */
		    invoker.invoke("MyNewRequest", ccbin, ccbout);
		    
		    /* Check */
		    assertEquals(100, ccbout.getDfhcommareaType().getComNumber());
		    assertEquals("$0100.11", ccbout.getDfhcommareaType().getComAmount());
		    assertEquals("*********", ccbout.getDfhcommareaType().getComComment());
		    assertEquals("26 11 81", ccbout.getDfhcommareaType().getComDate());
		    assertEquals("SURREY, ENGLAND", ccbout.getDfhcommareaType().getComPersonal().getComAddress());
		    assertEquals("S. D. BORMAN", ccbout.getDfhcommareaType().getComPersonal().getComName());
		    assertEquals("32156778", ccbout.getDfhcommareaType().getComPersonal().getComPhone());
		} catch (HostInvokerException e) {
			fail("testValidInvokeCommareaOverHttp failed " + e);
		}
	}
	
}
