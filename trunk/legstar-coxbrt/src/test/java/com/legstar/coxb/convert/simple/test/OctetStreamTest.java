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
package com.legstar.coxb.convert.simple.test;

import com.legstar.coxb.convert.simple.CobolOctetStreamSimpleConverter;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;

public class OctetStreamTest extends TestCase {
	
	public void testToHostSimple () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[4];
   	
    	byte[] javaBytes = {0x0c, 0x15, -0x17, 0x00};
		assertEquals(4, CobolOctetStreamSimpleConverter.toHostSingle(javaBytes,4, hostBytes, 0));
		assertEquals("0c15e900", HostData.toHexString(hostBytes));
	}

	public void testFromHostSimple () throws HostException{
		// Create a host buffer
		byte[] hostSource = {-0x3F, -0x3E, -0x3D, -0x3C};
   	
    	byte[] javaBytes = CobolOctetStreamSimpleConverter.fromHostSingle(4, hostSource, 0);
		assertEquals(hostSource[0], javaBytes[0]);
		assertEquals(hostSource[1], javaBytes[1]);
		assertEquals(hostSource[2], javaBytes[2]);
		assertEquals(hostSource[3], javaBytes[3]);
	}

}
