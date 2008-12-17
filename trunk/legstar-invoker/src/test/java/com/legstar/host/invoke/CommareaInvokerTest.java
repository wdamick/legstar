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

import com.legstar.coxb.host.HostData;
import com.legstar.host.invoke.HostInvokerException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaBinding;

/**
 * Test CommareaInvoker.
 */
public class CommareaInvokerTest extends AbstractTestInvokers {
	
	/** Test a successful access to LSFIELAE. */
    public void testValidInvokeCommarea()  {
		try {
			LegStarAddress address = new LegStarAddress("TheMainframe");
			address.setHostUserID(HOST_USERID);
			address.setHostPassword(HOST_PASSWORD);
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "lsfileae.properties");
			DfhcommareaBinding ccbout = invokeLsfileaeWithBinding(invoker);
		    checkLsfileaeJavaReply(ccbout.getDfhcommarea());
		    byte[] responseBytes = invokeLsfileae(invoker);
	        assertEquals(LSFILEAE_BYTES_REPLY, HostData.toHexString(responseBytes));
		    
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
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "wrongprog.properties");
			invokeLsfileae(invoker);
			fail("testWrongProgInvokeCommarea failed ");

		} catch (HostInvokerException e) {
			assertTrue(e.getMessage().contains("com.legstar.host.access.HostAccessStrategyException: com.legstar.messaging.RequestException: CICS command=LINK COMMAREA failed, resp=PGMIDERR, resp2=") );
		}
	}
	
	/** When passed an empty address, the parameters should come from the base configuration */
    public void testEmptyAddressWithBinding() {
		try {
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, null, "lsfileae.properties");
            DfhcommareaBinding ccbout = invokeLsfileaeWithBinding(invoker);
            checkLsfileaeJavaReply(ccbout.getDfhcommarea());
            byte[] responseBytes = invokeLsfileae(invoker);
            assertEquals(LSFILEAE_BYTES_REPLY, HostData.toHexString(responseBytes));
            
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
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "lsfileae.properties");
            DfhcommareaBinding ccbout = invokeLsfileaeWithBinding(invoker);
            checkLsfileaeJavaReply(ccbout.getDfhcommarea());
            byte[] responseBytes = invokeLsfileae(invoker);
            assertEquals(LSFILEAE_BYTES_REPLY, HostData.toHexString(responseBytes));
            
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
			HostInvoker invoker = HostInvokerFactory.createHostInvoker("config4.xml", address, "lsfileae.properties");
            DfhcommareaBinding ccbout = invokeLsfileaeWithBinding(invoker);
            checkLsfileaeJavaReply(ccbout.getDfhcommarea());
            byte[] responseBytes = invokeLsfileae(invoker);
            assertEquals(LSFILEAE_BYTES_REPLY, HostData.toHexString(responseBytes));
            
		} catch (HostInvokerException e) {
			fail("testValidInvokeCommareaOverHttp failed " + e);
		}
	}
	
	/** 
	 * This tests invoke using the old style "all inclusive" way where binding is
	 * done by the invoket (actually handed over to LegStarMessageImpl).
	 * @param invoker the current invoker
	 * @throws HostInvokerException if invoke fails
	 *  */
	@SuppressWarnings("deprecation")
    private DfhcommareaBinding invokeLsfileaeWithBinding(final HostInvoker invoker) throws HostInvokerException {
        /* The JAXB input factory. */
        com.legstar.test.coxb.lsfileae.ObjectFactory jaxbInFactory =
              new com.legstar.test.coxb.lsfileae.ObjectFactory(); 
        
        /* The request java object tree */
        Dfhcommarea request = jaxbInFactory.createDfhcommarea();
        request.setComNumber(100L);
        
        /* Decorate object tree for static binding */
        DfhcommareaBinding ccbin = new DfhcommareaBinding(request);
        
        /* Prepare output object */
        DfhcommareaBinding ccbout =
              new DfhcommareaBinding();
        
        /* call */
        invoker.invoke("MyNewRequest", ccbin, ccbout);
        
        return ccbout;
	}
	
    /** 
     * This tests invoke using the new style "raw mainframe" way where binding is
     * done by the caller.
     * @param invoker the current invoker
     * @throws HostInvokerException if invoke fails
     *  */
    private byte[] invokeLsfileae(final HostInvoker invoker) throws HostInvokerException {
        
        /* call */
        return invoker.invoke("MyNewRequest", HostData.toByteArray(LSFILEAE_BYTES_REQUEST));
    }
}
