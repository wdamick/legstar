package com.legstar.host.invoke;

import java.util.LinkedHashMap;
import java.util.Map;

import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.host.HostException;
import com.legstar.messaging.Address;
import com.legstar.test.coxb.lsfileac.QueryDataType;
import com.legstar.test.coxb.lsfileac.QueryLimitType;
import com.legstar.test.coxb.lsfileac.bind.QueryDataTypeBinding;
import com.legstar.test.coxb.lsfileac.bind.QueryLimitTypeBinding;
import com.legstar.test.coxb.lsfileac.bind.ReplyDataTypeBinding;
import com.legstar.test.coxb.lsfileac.bind.ReplyStatusTypeBinding;

import junit.framework.TestCase;

public class ContainerInvokerTest extends TestCase {

	private static final String CONFIG_FILE = "config0.xml";

	public void testContainerInvoker() throws HostInvokerException {
		Address address = new Address("CICSTS31");
		HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");
		assertTrue(invoker instanceof com.legstar.host.invoke.ContainerInvoker);
	}

	public void testInvokeStringICobolComplexBindingICobolComplexBinding() {
		try {
			Address address = new Address("CICSTS31");
			HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");
			ICobolComplexBinding ccbin = null;
			ICobolComplexBinding ccbout = null;
			invoker.invoke("testInvokeStringICobolComplexBindingICobolComplexBinding", ccbin, ccbout);
			fail("Method should not be supported");
		} catch (HostInvokerException e) {
			assertEquals("Unsupported method for CICS containers", e.getMessage());
		}
	}

	public void test2ContainersIn2Out() throws HostInvokerException, HostException {
		Address address = new Address("CICSTS31");
		HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");
	    /* The JAXB input factory. */
	    com.legstar.test.coxb.lsfileac.ObjectFactory jaxbInFactory =
	          new com.legstar.test.coxb.lsfileac.ObjectFactory(); 
	    
	    /* There are 2 containers for request */
	    QueryDataType queryData	= jaxbInFactory.createQueryDataType();
	    queryData.setQueryName("S*");
	    queryData.setQueryAddress("*");
	    queryData.setQueryPhone("*");
	    
	    QueryLimitType queryLimit	= jaxbInFactory.createQueryLimitType();
	    queryLimit.setMaxElapseTime(5000); /* 5 seconds */
	    queryLimit.setMaxItemsRead(100);
	    
	    /* Decorate object trees for static binding */
	    QueryDataTypeBinding queryDataBin = new QueryDataTypeBinding(queryData);
	    QueryLimitTypeBinding queryLimitBin = new QueryLimitTypeBinding(queryLimit);
	    
	    /* Prepare output objects */
	    ReplyDataTypeBinding replyDataBin = new ReplyDataTypeBinding();
	    ReplyStatusTypeBinding replyStatusBin = new ReplyStatusTypeBinding();
	    
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
	    assertEquals(0, replyStatusBin.getReplyStatusType().getReplyType());
	    assertEquals(44, replyStatusBin.getReplyStatusType().getTotalItemsRead());
	    assertEquals("", replyStatusBin.getReplyStatusType().getReplyMessage().trim());
	    assertEquals(0, replyStatusBin.getReplyStatusType().getReplyResp());
	    assertEquals(0, replyStatusBin.getReplyStatusType().getReplyResp2());
	    assertEquals(5, replyDataBin.getReplyDataType().getReplyItemscount());
	    
	    assertEquals("S. D. BORMAN", replyDataBin.getReplyDataType().getReplyItem().get(0).getReplyPersonal().getReplyName());
	    assertEquals("SUSAN MALAIKA", replyDataBin.getReplyDataType().getReplyItem().get(1).getReplyPersonal().getReplyName());
	    assertEquals("SIR MICHAEL ROBERTS", replyDataBin.getReplyDataType().getReplyItem().get(2).getReplyPersonal().getReplyName());
	    assertEquals("S. P. RUSSELL", replyDataBin.getReplyDataType().getReplyItem().get(3).getReplyPersonal().getReplyName());
	    assertEquals("S.J. LAZENBY", replyDataBin.getReplyDataType().getReplyItem().get(4).getReplyPersonal().getReplyName());
	    
	}

	/* Without any input containers, the host program returns the entire content */
	public void test0ContainersIn2Out() throws HostInvokerException, HostException {
		Address address = new Address("CICSTS31");
		HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");
	    
	    /* Prepare output objects */
	    ReplyDataTypeBinding replyDataBin = new ReplyDataTypeBinding();
	    ReplyStatusTypeBinding replyStatusBin = new ReplyStatusTypeBinding();
	    
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
	    assertEquals(0, replyStatusBin.getReplyStatusType().getReplyType());
	    assertEquals(44, replyStatusBin.getReplyStatusType().getTotalItemsRead());
	    assertEquals("", replyStatusBin.getReplyStatusType().getReplyMessage().trim());
	    assertEquals(0, replyStatusBin.getReplyStatusType().getReplyResp());
	    assertEquals(0, replyStatusBin.getReplyStatusType().getReplyResp2());
	    assertEquals(44, replyDataBin.getReplyDataType().getReplyItemscount());
	    
	    assertEquals("S. D. BORMAN", replyDataBin.getReplyDataType().getReplyItem().get(0).getReplyPersonal().getReplyName());
	    assertEquals("J. T. CZAYKOWSKI", replyDataBin.getReplyDataType().getReplyItem().get(1).getReplyPersonal().getReplyName());
	    assertEquals("M. B. DOMBEY", replyDataBin.getReplyDataType().getReplyItem().get(2).getReplyPersonal().getReplyName());
	    assertEquals("A. I. HICKSON", replyDataBin.getReplyDataType().getReplyItem().get(3).getReplyPersonal().getReplyName());
	    assertEquals("ALAN TULIP", replyDataBin.getReplyDataType().getReplyItem().get(4).getReplyPersonal().getReplyName());
	    
	}
	
	/* When nothing is selected on the host, there is no data container back */
	public void test2ContainersIn1Out() throws HostInvokerException, HostException {
		Address address = new Address("CICSTS31");
		HostInvoker invoker = HostInvokerFactory.createHostInvoker(CONFIG_FILE, address, "container1.properties");
	    /* The JAXB input factory. */
	    com.legstar.test.coxb.lsfileac.ObjectFactory jaxbInFactory =
	          new com.legstar.test.coxb.lsfileac.ObjectFactory(); 
	    
	    /* There are 2 containers for request */
	    QueryDataType queryData	= jaxbInFactory.createQueryDataType();
	    queryData.setQueryName("Z*");
	    queryData.setQueryAddress("*");
	    queryData.setQueryPhone("*");
	    
	    QueryLimitType queryLimit	= jaxbInFactory.createQueryLimitType();
	    queryLimit.setMaxElapseTime(5000); /* 5 seconds */
	    queryLimit.setMaxItemsRead(100);
	    
	    /* Decorate object trees for static binding */
	    QueryDataTypeBinding queryDataBin = new QueryDataTypeBinding(queryData);
	    QueryLimitTypeBinding queryLimitBin = new QueryLimitTypeBinding(queryLimit);
	    
	    /* Prepare output objects */
	    ReplyDataTypeBinding replyDataBin = new ReplyDataTypeBinding();
	    ReplyStatusTypeBinding replyStatusBin = new ReplyStatusTypeBinding();
	    
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
	    assertEquals(0, replyStatusBin.getReplyStatusType().getReplyType());
	    assertEquals(44, replyStatusBin.getReplyStatusType().getTotalItemsRead());
	    assertEquals("NO CUSTOMER SATISFIES YOUR QUERY", replyStatusBin.getReplyStatusType().getReplyMessage().trim());
	    assertEquals(0, replyStatusBin.getReplyStatusType().getReplyResp());
	    assertEquals(0, replyStatusBin.getReplyStatusType().getReplyResp2());
	    
	    assertTrue(null == replyDataBin.getReplyDataType());
	    
	}

}
