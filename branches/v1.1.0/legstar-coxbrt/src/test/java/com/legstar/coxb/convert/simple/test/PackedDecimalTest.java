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

import java.math.BigDecimal;

import com.legstar.coxb.convert.simple.*;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

public class PackedDecimalTest extends TestCase {
	
	public void testToHost456790p00675 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[6];
   	
    	BigDecimal javaDecimal = new BigDecimal("+456790.00675");
		assertEquals(6, CobolPackedDecimalSimpleConverter.toHostSingle(javaDecimal, 6, 11, 5, false, hostBytes, 0));
		assertEquals("45679000675f", HostData.toHexString(hostBytes));
	}
	public void testFromHost456790p00675 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("45679000675f");
   	
		BigDecimal javaDecimal = CobolPackedDecimalSimpleConverter.fromHostSingle(6, 11, 5, hostBytes, 0);
		assertEquals("456790.00675", javaDecimal.toString());
	}

	public void testToHostM123456789012345p12 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[9];
   	
    	BigDecimal javaDecimal = new BigDecimal("-123456789012345.12");
		assertEquals(9, CobolPackedDecimalSimpleConverter.toHostSingle(javaDecimal, 9, 17, 2, true, hostBytes, 0));
		assertEquals("12345678901234512d", HostData.toHexString(hostBytes));
	}
	public void testFromHostM123456789012345p12 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("12345678901234512d");
   	
		BigDecimal javaDecimal = CobolPackedDecimalSimpleConverter.fromHostSingle(9, 17, 2, hostBytes, 0);
		assertEquals("-123456789012345.12", javaDecimal.toString());
	}

	public void testToHostZero () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[9];
   	
    	BigDecimal javaDecimal = new BigDecimal("0");
		assertEquals(9, CobolPackedDecimalSimpleConverter.toHostSingle(javaDecimal, 9, 17, 2, true, hostBytes, 0));
		assertEquals("00000000000000000c", HostData.toHexString(hostBytes));
	}
	public void testFromHostZero () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("00000000000000000c");
   	
		BigDecimal javaDecimal = CobolPackedDecimalSimpleConverter.fromHostSingle(9, 17, 2, hostBytes, 0);
		assertEquals("0.00", javaDecimal.toString());
	}

	public void testToHostMZero () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[9];
   	
    	BigDecimal javaDecimal = new BigDecimal("-0");
		assertEquals(9, CobolPackedDecimalSimpleConverter.toHostSingle(javaDecimal, 9, 17, 2, true, hostBytes, 0));
		assertEquals("00000000000000000c", HostData.toHexString(hostBytes));
	}

	public void testToHostM67000p56007 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[6];
   	
    	BigDecimal javaDecimal = new BigDecimal("-67000.56007");
		assertEquals(6, CobolPackedDecimalSimpleConverter.toHostSingle(javaDecimal, 6, 10, 5, true, hostBytes, 0));
		assertEquals("06700056007d", HostData.toHexString(hostBytes));
	}
	public void testFromHostM67000p56007 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("06700056007d");
   	
		BigDecimal javaDecimal = CobolPackedDecimalSimpleConverter.fromHostSingle(6, 10, 5, hostBytes, 0);
		assertEquals("-67000.56007", javaDecimal.toString());
	}

	public void testToHostM67000p56007Trunc () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[6];
   	
    	BigDecimal javaDecimal = new BigDecimal("-67000.56007");
		assertEquals(6, CobolPackedDecimalSimpleConverter.toHostSingle(javaDecimal, 6, 10, 2, true, hostBytes, 0));
		assertEquals("00006700056d", HostData.toHexString(hostBytes));
	}
	public void testFromHostM67000p56007Trunc () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("06700056007d");
   	
		BigDecimal javaDecimal = CobolPackedDecimalSimpleConverter.fromHostSingle(6, 10, 2, hostBytes, 0);
		assertEquals("-67000560.07", javaDecimal.toString());
	}

	public void testToHostSingleDigit () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[1];
   	
    	BigDecimal javaDecimal = new BigDecimal("1");
		assertEquals(1, CobolPackedDecimalSimpleConverter.toHostSingle(javaDecimal, 1, 1, 0, false, hostBytes, 0));
		assertEquals("1f", HostData.toHexString(hostBytes));
	}

	public void testFromHostSingleDigit () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("1f");
   	
		BigDecimal javaDecimal = CobolPackedDecimalSimpleConverter.fromHostSingle(1, 1, 0, hostBytes, 0);
		assertEquals("1", javaDecimal.toString());
	}

	public void testToHostp12345 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[4];
   	
    	BigDecimal javaDecimal = new BigDecimal(".12345");
		assertEquals(4, CobolPackedDecimalSimpleConverter.toHostSingle(javaDecimal, 4, 6, 5, false, hostBytes, 0));
		assertEquals("0012345f", HostData.toHexString(hostBytes));
	}

	public void testFromHostp12345 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("0012345f");
   	
		BigDecimal javaDecimal = CobolPackedDecimalSimpleConverter.fromHostSingle(4, 6, 5, hostBytes, 0);
		assertEquals("0.12345", javaDecimal.toString());
	}

	public void testToHostScaling () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[6];
   	
    	BigDecimal javaDecimal = new BigDecimal("1234567.89");
		assertEquals(6, CobolPackedDecimalSimpleConverter.toHostSingle(javaDecimal, 6, 10, 3, false, hostBytes, 0));
		assertEquals("01234567890f", HostData.toHexString(hostBytes));
	}

	public void testFromHostScaling () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("01234567890f");
   	
		BigDecimal javaDecimal = CobolPackedDecimalSimpleConverter.fromHostSingle(6, 10, 3, hostBytes, 0);
		assertEquals("1234567.890", javaDecimal.toString());
	}

	public void testToHostPadding () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[3];
   	
    	BigDecimal javaDecimal = new BigDecimal("12");
		assertEquals(3, CobolPackedDecimalSimpleConverter.toHostSingle(javaDecimal, 3, 5, 0, false, hostBytes, 0));
		assertEquals("00012f", HostData.toHexString(hostBytes));
	}

	public void testFromHostPadding () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("00012f");
   	
		BigDecimal javaDecimal = CobolPackedDecimalSimpleConverter.fromHostSingle(3, 5, 0, hostBytes, 0);
		assertEquals("12", javaDecimal.toString());
	}

	public void testToHostLarge () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[16];
   	
    	BigDecimal javaDecimal = new BigDecimal("1234567890123456789012345678901");
		assertEquals(16, CobolPackedDecimalSimpleConverter.toHostSingle(javaDecimal, 16, 31, 0, false, hostBytes, 0));
		assertEquals("1234567890123456789012345678901f", HostData.toHexString(hostBytes));
	}

	public void testFromHostLarge () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("1234567890123456789012345678901f");
   	
		BigDecimal javaDecimal = CobolPackedDecimalSimpleConverter.fromHostSingle(16, 31, 0, hostBytes, 0);
		assertEquals("1234567890123456789012345678901", javaDecimal.toString());
	}

	public void testFromHostInvalid () throws HostException{
    	try {
			// Create a host buffer with host input
			byte[] hostBytes = HostData.toByteArray("1A");
	   	
			CobolPackedDecimalSimpleConverter.fromHostSingle(1, 1, 0, hostBytes, 0);
			fail("Invalid packed decimal not detected");
    	}  	catch (HostException he) {
    		assertEquals("Host data last byte is not a valid packed decimal byte. Host data at offset 0=0x1a", he.getMessage());
    	}
	}

	public void testFromHostInvalid2 () throws HostException{
    	try {
			// Create a host buffer with host input
			byte[] hostBytes = HostData.toByteArray("0A1f");
	   	
			CobolPackedDecimalSimpleConverter.fromHostSingle(2, 2, 0, hostBytes, 0);
			fail("Invalid packed decimal not detected");
    	}  	catch (HostException he) {
    		assertEquals("Host data contains a byte that is not a valid packed decimal byte. Host data at offset 0=0x0a1f", he.getMessage());
    	}
	}

	public void testToHostInvalid () throws HostException{
    	try {
    		// Create a host buffer
    		byte[] hostBytes = new byte[1];
       	
        	BigDecimal javaDecimal = new BigDecimal("235.87");
    		CobolPackedDecimalSimpleConverter.toHostSingle(javaDecimal, 1, 1, 0, false, hostBytes, 0);
			fail("BigDecimal too large not detected");
    	} 	catch (HostException he) {
    		assertEquals("BigDecimal value too large for target Cobol field. Host data at offset 0=0x00", he.getMessage());
    	}
	}

}
