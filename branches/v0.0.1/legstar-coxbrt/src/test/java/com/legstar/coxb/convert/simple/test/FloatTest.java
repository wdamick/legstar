/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.coxb.convert.simple.test;

import com.legstar.coxb.convert.simple.CobolFloatSimpleConverter;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;

public class FloatTest extends TestCase {

	public void testToHost1234 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[4];
   	
    	Float javaFloat = new Float("+1234.0f");
		assertEquals(4, CobolFloatSimpleConverter.toHostSingle(javaFloat, hostBytes, 0));
		assertEquals("434d2000", HostData.toHexString(hostBytes));
	}

	public void testFromHost1234 () throws HostException{
		// Create a host buffer
		byte[] hostSource = {0x43, 0x4d, 0x20, 0x00};
   	
    	Float javaFloat = CobolFloatSimpleConverter.fromHostSingle(4, hostSource, 0);
		assertEquals("1234.0", javaFloat.toString());
	}

	public void testToHost0 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[4];
   	
    	Float javaFloat = new Float("+0.0f");
		assertEquals(4, CobolFloatSimpleConverter.toHostSingle(javaFloat, hostBytes, 0));
		assertEquals("00000000", HostData.toHexString(hostBytes));
	}

	public void testFromHost0 () throws HostException{
		// Create a host buffer
		byte[] hostSource = {0x00, 0x00, 0x00, 0x00};
   	
    	Float javaFloat = CobolFloatSimpleConverter.fromHostSingle(4, hostSource, 0);
		assertEquals("0.0", javaFloat.toString());
	}

	public void testToHost1 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[4];
   	
    	Float javaFloat = new Float("+1.0f");
		assertEquals(4, CobolFloatSimpleConverter.toHostSingle(javaFloat, hostBytes, 0));
		assertEquals("41100000", HostData.toHexString(hostBytes));
	}

	public void testFromHost1 () throws HostException{
		// Create a host buffer
		byte[] hostSource = {0x41, 0x10, 0x00, 0x00};
   	
    	Float javaFloat = CobolFloatSimpleConverter.fromHostSingle(4, hostSource, 0);
		assertEquals("1.0", javaFloat.toString());
	}

	public void testToHost345006p5678 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[4];
   	
    	Float javaFloat = new Float("+345006.5678f");
		assertEquals(4, CobolFloatSimpleConverter.toHostSingle(javaFloat, hostBytes, 0));
		assertEquals("45543ae9", HostData.toHexString(hostBytes));
	}

	public void testFromHost345006p5678 () throws HostException{
		// Create a host buffer
		byte[] hostSource = {0x45, 0x54, 0x3a, -0x17};
   	
    	Float javaFloat = CobolFloatSimpleConverter.fromHostSingle(4, hostSource, 0);
		assertEquals("345006.56", javaFloat.toString());
	}

	public void testToHost798p20067em16 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[4];
   	
    	Float javaFloat = new Float("+798.20067e-16f");
		assertEquals(4, CobolFloatSimpleConverter.toHostSingle(javaFloat, hostBytes, 0));
		assertEquals("361677a4", HostData.toHexString(hostBytes));
	}

	public void testFromHost798p20067em16 () throws HostException{
		// Create a host buffer
		byte[] hostSource = {0x36, 0x16, 0x77, -0x5c};
   	
    	Float javaFloat = CobolFloatSimpleConverter.fromHostSingle(4, hostSource, 0);
		assertEquals("7.982005E-14", javaFloat.toString());
	}

	public void testToHost3p40282347ep38 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[4];
   	
    	Float javaFloat = new Float("3.40282347e+38f");
		assertEquals(4, CobolFloatSimpleConverter.toHostSingle(javaFloat, hostBytes, 0));
		assertEquals("60ffffff", HostData.toHexString(hostBytes));
	}

	public void testFromHost3p40282347ep38 () throws HostException{
		// Create a host buffer
		byte[] hostSource = {0x60, -0x01, -0x01, -0x01};
   	
    	Float javaFloat = CobolFloatSimpleConverter.fromHostSingle(4, hostSource, 0);
		assertEquals("3.4028235E38", javaFloat.toString());
	}
}
