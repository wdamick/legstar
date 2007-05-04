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

import com.legstar.coxb.convert.simple.CobolDoubleSimpleConverter;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;

public class DoubleTest extends TestCase {

	public void testToHost1234 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[8];
   	
    	Double javaDouble = new Double("+1234.0d");
		assertEquals(8, CobolDoubleSimpleConverter.toHostSingle(javaDouble, hostBytes, 0));
		assertEquals("434d200000000000", HostData.toHexString(hostBytes));
	}

	public void testFromHost1234 () throws HostException{
		// Create a host buffer
		byte[] hostSource = {0x43, 0x4d, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00};
   	
    	Double javaDouble = CobolDoubleSimpleConverter.fromHostSingle(8, hostSource, 0);
		assertEquals("1234.0", javaDouble.toString());
	}

	public void testToHost0 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[8];
   	
    	Double javaDouble = new Double("+0.0d");
		assertEquals(8, CobolDoubleSimpleConverter.toHostSingle(javaDouble, hostBytes, 0));
		assertEquals("0000000000000000", HostData.toHexString(hostBytes));
	}

	public void testFromHost0 () throws HostException{
		// Create a host buffer
		byte[] hostSource = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
   	
    	Double javaDouble = CobolDoubleSimpleConverter.fromHostSingle(8, hostSource, 0);
		assertEquals("0.0", javaDouble.toString());
	}

	public void testToHost1 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[8];
   	
    	Double javaDouble = new Double("+1.0d");
		assertEquals(8, CobolDoubleSimpleConverter.toHostSingle(javaDouble, hostBytes, 0));
		assertEquals("4110000000000000", HostData.toHexString(hostBytes));
	}

	public void testFromHost1 () throws HostException{
		// Create a host buffer
		byte[] hostSource = {0x41, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
   	
    	Double javaDouble = CobolDoubleSimpleConverter.fromHostSingle(8, hostSource, 0);
		assertEquals("1.0", javaDouble.toString());
	}

	public void testToHost345006p5678 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[8];
   	
    	Double javaDouble = new Double("+345006.5678d");
		assertEquals(8, CobolDoubleSimpleConverter.toHostSingle(javaDouble, hostBytes, 0));
		assertEquals("45543ae915b573e0", HostData.toHexString(hostBytes));
	}

	public void testFromHost345006p5678 () throws HostException{
		// Create a host buffer
		byte[] hostSource = {0x45, 0x54, 0x3a, -0x17, 0x15, -0x4b, 0x73, -0x19};
   	
    	Double javaDouble = CobolDoubleSimpleConverter.fromHostSingle(8, hostSource, 0);
		assertEquals("345006.56779999984", javaDouble.toString());
	}

	public void testToHost798p20067em16 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[8];
   	
    	Double javaDouble = new Double("+798.20067e-16d");
		assertEquals(8, CobolDoubleSimpleConverter.toHostSingle(javaDouble, hostBytes, 0));
		assertEquals("361677a4590fab60", HostData.toHexString(hostBytes));
	}

	public void testFromHost798p20067em16 () throws HostException{
		// Create a host buffer
		byte[] hostSource = {0x36, 0x16, 0x77, -0x5c, 0x59, 0x0f, -0x55, 0x60};
   	
    	Double javaDouble = CobolDoubleSimpleConverter.fromHostSingle(8, hostSource, 0);
		assertEquals("7.982006699999985E-14", javaDouble.toString());
	}

	public void testToHost3p40282347ep38 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[8];
   	
    	Double javaDouble = new Double("3.40282347e+38d");
		assertEquals(8, CobolDoubleSimpleConverter.toHostSingle(javaDouble, hostBytes, 0));
		assertEquals("60ffffff048ff9e0", HostData.toHexString(hostBytes));
	}

	public void testFromHost3p40282347ep38 () throws HostException{
		// Create a host buffer
		byte[] hostSource = {0x60, -0x01, -0x01, -0x01, 0x04, -0x71, -0x07, -0x20};
   	
    	Double javaDouble = CobolDoubleSimpleConverter.fromHostSingle(8, hostSource, 0);
		assertEquals("3.4028234699999995E38", javaDouble.toString());
	}

	public void testToHostm5p670078Em14 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[8];
   	
    	Double javaDouble = new Double("-5.670078E-14d");
		assertEquals(8, CobolDoubleSimpleConverter.toHostSingle(javaDouble, hostBytes, 0));
		assertEquals("b60ff5b8c70649e0", HostData.toHexString(hostBytes));
	}

	public void testFromHostm5p670078Em14 () throws HostException{
		// Create a host buffer
		byte[] hostSource = HostData.toByteArray("b5ff5b8c70649ed1");
   	
    	Double javaDouble = CobolDoubleSimpleConverter.fromHostSingle(8, hostSource, 0);
		assertEquals("-5.670078E-14", javaDouble.toString());
	}
}
