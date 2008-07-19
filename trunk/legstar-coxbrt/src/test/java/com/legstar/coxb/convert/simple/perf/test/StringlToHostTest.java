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
package com.legstar.coxb.convert.simple.perf.test;

import com.legstar.coxb.convert.simple.CobolStringSimpleConverter;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

public class StringlToHostTest extends TestCase {
	
	private static int ITERATIONS = 100;
	public void testToHost() throws HostException {
    	String frenchCharSet = "IBM01147"; // French charset
    	String javaString = "ça c'est un problème élémentaire à résoudre";
		// Create a host buffer
		byte[] hostBytes = new byte[43];
		for (int i = 0; i < ITERATIONS; i++) {
			assertEquals(43, CobolStringSimpleConverter.toHostSingle(javaString, frenchCharSet, 43,false, hostBytes, 0));
			assertEquals("e08140837d85a2a340a495409799968293d0948540c093c0948595a381899985407c4099c0a296a4849985", HostData.toHexString(hostBytes));
		}
	}

}
