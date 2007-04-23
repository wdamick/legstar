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
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;

public class ZonedDecimalTest extends TestCase {
	
	public void testToHost456790p00675 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[11];
   	
    	BigDecimal javaDecimal = new BigDecimal("+456790.00675");
    	/* toHostSingle BigDecimal javaDecimal,
			int cobolByteLength,
			int totalDigits,
			int fractionDigits,
			boolean isSigned,
			boolean isSignSeparate,
			boolean isSignLeading,
			byte[] hostTarget,
			int offset */
		assertEquals(11, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 11, 11, 5, false, false, true, hostBytes, 0));
		assertEquals("f4f5f6f7f9f0f0f0f6f7f5", HostData.toHexString(hostBytes));
	}
	public void testFromHost456790p00675 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f4f5f6f7f9f0f0f0f6f7f5");
   	
		/* fromHostSingle int hostLength,
			int totalDigits,
			int fractionDigits,
			boolean isSigned,
			boolean isSignSeparate,
			boolean isSignLeading,
			byte[] hostSource,
			int offset*/
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(11, 11, 5, false, false, true, hostBytes, 0);
		assertEquals("456790.00675", javaDecimal.toString());
	}

	public void testToHostM123456789012345p12 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[17];
   	
    	BigDecimal javaDecimal = new BigDecimal("-123456789012345.12");
		assertEquals(17, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 17, 17, 2, true, false, false, hostBytes, 0));
		assertEquals("f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f1d2", HostData.toHexString(hostBytes));
	}
	public void testFromHostM123456789012345p12 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f1d2");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(17, 17, 2, true, false, false, hostBytes, 0);
		assertEquals("-123456789012345.12", javaDecimal.toString());
	}

	public void testToHostZero () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[17];
   	
    	BigDecimal javaDecimal = new BigDecimal("0");
		assertEquals(17, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 17, 17, 0, false, false, false, hostBytes, 0));
		assertEquals("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0", HostData.toHexString(hostBytes));
	}
	public void testFromHostZero () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(17, 17, 0, false, false, false, hostBytes, 0);
		assertEquals("0", javaDecimal.toString());
	}

	public void testToHostMZero () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[9];
   	
    	BigDecimal javaDecimal = new BigDecimal("-0");
		assertEquals(9, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 9, 9, 0, false, false, true, hostBytes, 0));
		assertEquals("f0f0f0f0f0f0f0f0f0", HostData.toHexString(hostBytes));
	}

	public void testToHostM67000p56007 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[11];
   	
    	BigDecimal javaDecimal = new BigDecimal("-67000.56007");
		assertEquals(11, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 11, 10, 5, true, true, true, hostBytes, 0));
		assertEquals("60f6f7f0f0f0f5f6f0f0f7", HostData.toHexString(hostBytes));
	}
	public void testFromHostM67000p56007 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("60f6f7f0f0f0f5f6f0f0f7");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(11, 10, 5, true, true, true, hostBytes, 0);
		assertEquals("-67000.56007", javaDecimal.toString());
	}

	public void testToHostM67000p56007Trunc () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[10];
   	
    	BigDecimal javaDecimal = new BigDecimal("-67000.56007");
		assertEquals(10, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 10, 10, 2, true, false, false, hostBytes, 0));
		assertEquals("f0f0f0f6f7f0f0f0f5d6", HostData.toHexString(hostBytes));
	}
	public void testFromHostM67000p56007Trunc () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f0f0f0f6f7f0f0f0f5d6");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(10, 10, 2, true, false, false, hostBytes, 0);
		assertEquals("-67000.56", javaDecimal.toString());
	}

	public void testToHostSingleDigit () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[1];
   	
    	BigDecimal javaDecimal = new BigDecimal("1");
		assertEquals(1, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 1, 1, 0, false, false, false, hostBytes, 0));
		assertEquals("f1", HostData.toHexString(hostBytes));
	}

	public void testFromHostSingleDigit () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f1");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(1, 1, 0, false, false, true, hostBytes, 0);
		assertEquals("1", javaDecimal.toString());
	}

	public void testToHostp12345 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[6];
   	
    	BigDecimal javaDecimal = new BigDecimal(".12345");
		assertEquals(6, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 6, 6, 5, false, false, false, hostBytes, 0));
		assertEquals("f0f1f2f3f4f5", HostData.toHexString(hostBytes));
	}

	public void testFromHostp12345 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f0f1f2f3f4f5");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(6, 6, 5, false, false, true, hostBytes, 0);
		assertEquals("0.12345", javaDecimal.toString());
	}

	public void testToHostScaling () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[10];
   	
    	BigDecimal javaDecimal = new BigDecimal("1234567.89");
		assertEquals(10, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 10, 10, 3, false, false, false, hostBytes, 0));
		assertEquals("f1f2f3f4f5f6f7f8f9f0", HostData.toHexString(hostBytes));
	}

	public void testFromHostScaling () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f1f2f3f4f5f6f7f8f9f0");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(10, 10, 3, false, false, true, hostBytes, 0);
		assertEquals("1234567.890", javaDecimal.toString());
	}

	public void testToHostPadding () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[5];
   	
    	BigDecimal javaDecimal = new BigDecimal("12");
		assertEquals(5, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 5, 5, 0, false, false, false, hostBytes, 0));
		assertEquals("f0f0f0f1f2", HostData.toHexString(hostBytes));
	}

	public void testFromHostPadding () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f0f0f0f1f2");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(5, 5, 0, false, false, true, hostBytes, 0);
		assertEquals("12", javaDecimal.toString());
	}

	public void testToHostLarge () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[31];
   	
    	BigDecimal javaDecimal = new BigDecimal("1234567890123456789012345678901");
		assertEquals(31, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 31, 31, 0, false, false, false, hostBytes, 0));
		assertEquals("f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f6f7f8f9f0f1", HostData.toHexString(hostBytes));
	}

	public void testFromHostLarge () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f6f7f8f9f0f1");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(31, 31, 0, false, false, true, hostBytes, 0);
		assertEquals("1234567890123456789012345678901", javaDecimal.toString());
	}

	public void testToHostSignSepLead () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[9];
   	
    	BigDecimal javaDecimal = new BigDecimal("-45.98754");
		assertEquals(9, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 9, 8, 6, true, true, true, hostBytes, 0));
		assertEquals("60f4f5f9f8f7f5f4f0", HostData.toHexString(hostBytes));
	}

	public void testFromSignSepLead () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("60f4f5f9f8f7f5f4f0");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(9, 8, 6, true, true, true, hostBytes, 0);
		assertEquals("-45.987540", javaDecimal.toString());
	}

	public void testToHostSignLead () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[8];
   	
    	BigDecimal javaDecimal = new BigDecimal("-45.98754");
		assertEquals(8, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 8, 8, 6, true, false, true, hostBytes, 0));
		assertEquals("d4f5f9f8f7f5f4f0", HostData.toHexString(hostBytes));
	}

	public void testFromSignLead () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("d4f5f9f8f7f5f4f0");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(8, 8, 6, true, false, true, hostBytes, 0);
		assertEquals("-45.987540", javaDecimal.toString());
	}

	public void testToHostSignSepTrail () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[9];
   	
    	BigDecimal javaDecimal = new BigDecimal("-45.98754");
		assertEquals(9, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 9, 8, 6, true, true, false, hostBytes, 0));
		assertEquals("f4f5f9f8f7f5f4f060", HostData.toHexString(hostBytes));
	}

	public void testFromSignSepTrail () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f4f5f9f8f7f5f4f060");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(9, 8, 6, true, true, false, hostBytes, 0);
		assertEquals("-45.987540", javaDecimal.toString());
	}

	public void testToHostSignTrail () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[8];
   	
    	BigDecimal javaDecimal = new BigDecimal("-45.98754");
		assertEquals(8, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 8, 8, 6, true, false, false, hostBytes, 0));
		assertEquals("f4f5f9f8f7f5f4d0", HostData.toHexString(hostBytes));
	}

	public void testFromSignTrail () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f4f5f9f8f7f5f4d0");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(8, 8, 6, true, false, false, hostBytes, 0);
		assertEquals("-45.987540", javaDecimal.toString());
	}
	public void testFromHostInvalid () throws HostException{
    	try {
			// Create a host buffer with host input
			byte[] hostBytes = HostData.toByteArray("1A");
	   	
			CobolZonedDecimalSimpleConverter.fromHostSingle(1, 1, 0, false, false, true, hostBytes, 0);
			fail("Invalid zoned decimal not detected");
    	} 	catch (HostException he) {
    		assertEquals("Host data contains a byte that is not a valid zoned decimal byte. Host data at offset 0=0x1a", he.getMessage());
    	}
	}

	public void testFromHostInvalid2 () throws HostException{
    	try {
			// Create a host buffer with host input
			byte[] hostBytes = HostData.toByteArray("0A1f");
	   	
			CobolZonedDecimalSimpleConverter.fromHostSingle(2, 2, 0, false, false, true, hostBytes, 0);
			fail("Invalid zoned decimal not detected");
    	}  	catch (HostException he) {
    		assertEquals("Host data contains a byte that is not a valid zoned decimal byte. Host data at offset 0=0x0a1f", he.getMessage());
    	}
	}

	public void testToHostInvalid () throws HostException{
    	try {
    		// Create a host buffer
    		byte[] hostBytes = new byte[1];
       	
        	BigDecimal javaDecimal = new BigDecimal("235.87");
    		CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 1, 1, 0, false, false, false, hostBytes, 0);
			fail("BigDecimal too large not detected");
    	}  	catch (HostException he) {
    		assertEquals("BigDecimal value too large for target Cobol field. Host data at offset 0=0x00", he.getMessage());
    	}
	}

}
