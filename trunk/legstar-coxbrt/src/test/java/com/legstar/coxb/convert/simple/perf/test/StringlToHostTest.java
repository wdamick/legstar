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
