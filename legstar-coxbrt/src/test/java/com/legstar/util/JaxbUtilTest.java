package com.legstar.util;

import junit.framework.TestCase;

public class JaxbUtilTest extends TestCase {
	
	public void testGetJavaClassNameAnnotations() throws Exception {
		assertEquals("com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply",
				JaxbUtil.getJavaClassName(
				"com.legstar.test.coxb.jvmquery", "JvmQueryReplyType"));
		
	}

	public void testGetJavaClassNameNoAnnotations() throws Exception {
		assertEquals("com.legstar.test.coxb.dplarcht.DfhcommareaType",
				JaxbUtil.getJavaClassName(
				"com.legstar.test.coxb.dplarcht", "DfhcommareaType"));
		
	}
}
