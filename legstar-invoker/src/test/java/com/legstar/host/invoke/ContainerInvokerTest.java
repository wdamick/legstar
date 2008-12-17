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
import com.legstar.messaging.LegStarAddress;
import com.legstar.test.coxb.lsfileac.QueryData;
import com.legstar.test.coxb.lsfileac.QueryLimit;
import com.legstar.test.coxb.lsfileac.bind.QueryDataBinding;
import com.legstar.test.coxb.lsfileac.bind.QueryLimitBinding;
import com.legstar.test.coxb.lsfileac.bind.ReplyDataBinding;
import com.legstar.test.coxb.lsfileac.bind.ReplyStatusBinding;

/**
 * Test ContainerInvoker.
 */
public class ContainerInvokerTest extends AbstractTestInvokers {

	/** Check that the factory correctly returns a containerInvoker. */
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

	/** Test with 2 input containers and 2 output containers (old style). */
    public void test2ContainersIn2OutWithBinding() throws HostInvokerException, HostException {
		LegStarAddress address = new LegStarAddress("CICSTS31");
		HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");
	    /* The JAXB input factory. */
	    com.legstar.test.coxb.lsfileac.ObjectFactory jaxbInFactory =
	          new com.legstar.test.coxb.lsfileac.ObjectFactory(); 
	    
	    /* There are 2 containers for request */
	    QueryData queryData	= jaxbInFactory.createQueryData();
	    queryData.setQueryName("S*");
	    queryData.setQueryAddress("*");
	    queryData.setQueryPhone("*");
	    
	    QueryLimit queryLimit	= jaxbInFactory.createQueryLimit();
	    queryLimit.setMaxElapseTime(5000); /* 5 seconds */
	    queryLimit.setMaxItemsRead(100);
	    
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
	    assertEquals(0, replyStatusBin.getReplyStatus().getReplyType());
	    assertEquals(44, replyStatusBin.getReplyStatus().getTotalItemsRead());
	    assertEquals("", replyStatusBin.getReplyStatus().getReplyMessage().trim());
	    assertEquals(0, replyStatusBin.getReplyStatus().getReplyResp());
	    assertEquals(0, replyStatusBin.getReplyStatus().getReplyResp2());
	    assertEquals(5, replyDataBin.getReplyData().getReplyItemscount());
	    
	    assertEquals("S. D. BORMAN", replyDataBin.getReplyData().getReplyItem().get(0).getReplyPersonal().getReplyName());
	    assertEquals("SUSAN MALAIKA", replyDataBin.getReplyData().getReplyItem().get(1).getReplyPersonal().getReplyName());
	    assertEquals("SIR MICHAEL ROBERTS", replyDataBin.getReplyData().getReplyItem().get(2).getReplyPersonal().getReplyName());
	    assertEquals("S. P. RUSSELL", replyDataBin.getReplyData().getReplyItem().get(3).getReplyPersonal().getReplyName());
	    assertEquals("S.J. LAZENBY", replyDataBin.getReplyData().getReplyItem().get(4).getReplyPersonal().getReplyName());
	    
	}

    /** Test with 2 input containers and 2 output containers (new style). */
    public void test2ContainersIn2Out() throws HostInvokerException, HostException {
        LegStarAddress address = new LegStarAddress("CICSTS31");
        HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");
        
        /* Get raw mainframe bytes */
        byte[] queryDataBin = HostData.toByteArray(LSFILEAC_QUERYDATA_BYTES);
        byte[] queryLimitBin = HostData.toByteArray(LSFILEAC_QUERYLIMIT_BYTES);
        
        /* Map containers with corresponding byte arrays */
        Map < String, byte[] > inParts =
            new LinkedHashMap < String, byte[] >(); 
        inParts.put("QueryData", queryDataBin);
        inParts.put("QueryLimit", queryLimitBin);
        
        /* call */
        Map < String, byte[] > outParts = invoker.invoke("test2ContainersIn2Out", inParts);
        
        /* Check */
        assertTrue(outParts != null);
        assertEquals(2, outParts.size());
        assertEquals(LSFILEAC_QUERYREPLY_BYTES, HostData.toHexString(outParts.get("ReplyData")));
        assertEquals(LSFILEAC_REPLYSTATUS_BYTES, HostData.toHexString(outParts.get("ReplyStatus")));
        
    }

    /** Without any input containers, the host program returns the entire content (old style) */
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
	    assertEquals(0, replyStatusBin.getReplyStatus().getReplyType());
	    assertEquals(44, replyStatusBin.getReplyStatus().getTotalItemsRead());
	    assertEquals("", replyStatusBin.getReplyStatus().getReplyMessage().trim());
	    assertEquals(0, replyStatusBin.getReplyStatus().getReplyResp());
	    assertEquals(0, replyStatusBin.getReplyStatus().getReplyResp2());
	    assertEquals(44, replyDataBin.getReplyData().getReplyItemscount());
	    
	    assertEquals("S. D. BORMAN", replyDataBin.getReplyData().getReplyItem().get(0).getReplyPersonal().getReplyName());
	    assertEquals("J. T. CZAYKOWSKI", replyDataBin.getReplyData().getReplyItem().get(1).getReplyPersonal().getReplyName());
	    assertEquals("M. B. DOMBEY", replyDataBin.getReplyData().getReplyItem().get(2).getReplyPersonal().getReplyName());
	    assertEquals("A. I. HICKSON", replyDataBin.getReplyData().getReplyItem().get(3).getReplyPersonal().getReplyName());
	    assertEquals("ALAN TULIP", replyDataBin.getReplyData().getReplyItem().get(4).getReplyPersonal().getReplyName());
	    
	}
	
    /** Without any input containers, the host program returns the entire content (new style) */
    public void test0ContainersIn2Out() throws HostInvokerException, HostException {
        LegStarAddress address = new LegStarAddress("CICSTS31");
        HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");
        
        /* Map containers with corresponding byte arrays */
        Map < String, byte[] > inParts =
            new LinkedHashMap < String, byte[] >(); 
        
        /* call */
        Map < String, byte[] > outParts = invoker.invoke("test2ContainersIn2Out", inParts);
        
        /* Check */
        assertTrue(outParts != null);
        assertEquals(2, outParts.size());
        assertEquals(3481, outParts.get("ReplyData").length);
        assertEquals(LSFILEAC_REPLYSTATUS_BYTES, HostData.toHexString(outParts.get("ReplyStatus")));
        
    }

	/** When nothing is selected on the host, there is no data container back (old style) */
	public void test2ContainersIn1OutWithBinding() throws HostInvokerException, HostException {
		LegStarAddress address = new LegStarAddress("CICSTS31");
		HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");
	    /* The JAXB input factory. */
	    com.legstar.test.coxb.lsfileac.ObjectFactory jaxbInFactory =
	          new com.legstar.test.coxb.lsfileac.ObjectFactory(); 
	    
	    /* There are 2 containers for request */
	    QueryData queryData	= jaxbInFactory.createQueryData();
	    queryData.setQueryName("Z*");
	    queryData.setQueryAddress("*");
	    queryData.setQueryPhone("*");
	    
	    QueryLimit queryLimit	= jaxbInFactory.createQueryLimit();
	    queryLimit.setMaxElapseTime(5000); /* 5 seconds */
	    queryLimit.setMaxItemsRead(100);
	    
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
	    assertEquals(0, replyStatusBin.getReplyStatus().getReplyType());
	    assertEquals(44, replyStatusBin.getReplyStatus().getTotalItemsRead());
	    assertEquals("NO CUSTOMER SATISFIES YOUR QUERY", replyStatusBin.getReplyStatus().getReplyMessage().trim());
	    assertEquals(0, replyStatusBin.getReplyStatus().getReplyResp());
	    assertEquals(0, replyStatusBin.getReplyStatus().getReplyResp2());
	    
	    assertTrue(null == replyDataBin.getReplyData());
	    
	}

    /** When nothing is selected on the host, there is no data container back (new style) */
    public void test2ContainersIn1Out() throws HostInvokerException, HostException {
        LegStarAddress address = new LegStarAddress("CICSTS31");
        HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");
        
        /* Get raw mainframe bytes */
        byte[] queryDataBin = HostData.toByteArray(LSFILEAC_QUERYDATA_BYTES.replace("e2", "e9"));
        byte[] queryLimitBin = HostData.toByteArray(LSFILEAC_QUERYLIMIT_BYTES);
        
        /* Map containers with corresponding byte arrays */
        Map < String, byte[] > inParts =
            new LinkedHashMap < String, byte[] >(); 
        inParts.put("QueryData", queryDataBin);
        inParts.put("QueryLimit", queryLimitBin);
        
        /* call */
        Map < String, byte[] > outParts = invoker.invoke("test2ContainersIn2Out", inParts);
        
        /* Check */
        assertTrue(outParts != null);
        assertEquals(2, outParts.size());
        assertEquals(null, outParts.get("ReplyData"));
        assertEquals(LSFILEAC_REPLYSTATUS_NOMATCH_BYTES, HostData.toHexString(outParts.get("ReplyStatus")));
        
    }

}
