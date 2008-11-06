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
package com.legstar.util;

import junit.framework.TestCase;

public class JaxbUtilTest extends TestCase {
	
	public void testGetJavaClassNameAnnotations() throws Exception {
		assertEquals("com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply",
				JaxbUtil.getJavaClassName(
				"com.legstar.test.coxb.jvmquery", "JvmQueryReply"));
		
	}

	public void testGetJavaClassNameNoAnnotations() throws Exception {
		assertEquals("com.legstar.test.coxb.dplarcht.DfhcommareaType",
				JaxbUtil.getJavaClassName(
				"com.legstar.test.coxb.dplarcht", "DfhcommareaType"));
		
	}
}
