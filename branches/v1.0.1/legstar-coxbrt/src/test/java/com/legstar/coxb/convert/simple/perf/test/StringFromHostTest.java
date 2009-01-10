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
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;

public class StringFromHostTest extends TestCase {
	
	private static int ITERATIONS = 100;
	public void testToHost() throws HostException {
    	String frenchCharSet = "IBM01147"; // French charset
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("e08140837d85a2a340a495409799968293d0948540c093c0948595a381899985407c4099c0a296a4849985");
   	
		for (int i = 0; i < ITERATIONS; i++) {
			String javaString = CobolStringSimpleConverter.fromHostSingle(frenchCharSet, 43,hostBytes, 0);
			assertEquals("�a c'est un probl�me �l�mentaire � r�soudre", javaString.toString());
		}
	}

}