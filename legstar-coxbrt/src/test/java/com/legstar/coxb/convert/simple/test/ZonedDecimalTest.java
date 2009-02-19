/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.convert.simple.test;

import java.math.BigDecimal;

import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.convert.simple.CobolZonedDecimalSimpleConverter;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

public class ZonedDecimalTest extends TestCase {
	
	public void testScaling() {
		BigDecimal decimal = new BigDecimal(256.85);
		assertEquals("000000000025685000", CobolZonedDecimalSimpleConverter.formatString(decimal, 18, 5, false, false, false));
		decimal = new BigDecimal(256.852568);
		assertEquals("000000000025685256", CobolZonedDecimalSimpleConverter.formatString(decimal, 18, 5, false, false, false));
		decimal = new BigDecimal(256.85257);
		assertEquals("000000000025685257", CobolZonedDecimalSimpleConverter.formatString(decimal, 18, 5, false, false, false));
		decimal = new BigDecimal(256.85257);
		assertEquals("000000000000000256", CobolZonedDecimalSimpleConverter.formatString(decimal, 18, 0, false, false, false));
	}

	public void testVirtualizing() {
		BigDecimal decimal = new BigDecimal(.256);
		assertEquals("000000000000025600", CobolZonedDecimalSimpleConverter.formatString(decimal, 18, 5, false, false, false));
	}

	public void testSignSeparate() {
		BigDecimal decimal = new BigDecimal("456");
		assertEquals("+000000456", CobolZonedDecimalSimpleConverter.formatString(decimal, 9, 0, true, true, true));
		decimal = new BigDecimal("456");
		assertEquals("000000456+", CobolZonedDecimalSimpleConverter.formatString(decimal, 9, 0, true, true, false));
		decimal = new BigDecimal("-456");
		assertEquals("-000000456", CobolZonedDecimalSimpleConverter.formatString(decimal, 9, 0, true, true, true));
		decimal = new BigDecimal("-456");
		assertEquals("000000456-", CobolZonedDecimalSimpleConverter.formatString(decimal, 9, 0, true, true, false));
	}
	
	public void testHostCharset()throws HostException {
		byte[] hostBytes = new byte[18];
    	BigDecimal javaDecimal = new BigDecimal("-123456789012345.12");
		assertEquals(18, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 17, 17, 2, true, true, true, hostBytes, 0, "ISO-8859-1"));
		assertEquals("2d3132333435363738393031323334353132", HostData.toHexString(hostBytes));
	}

	public void testOverflow() {
		byte[] hostBytes = new byte[9];
		BigDecimal decimal = new BigDecimal("25689745623");
		try {
			CobolZonedDecimalSimpleConverter.toHostSingle(
					decimal, 9, 9, 0, false, false, false, hostBytes, 0, "IBM01140");
		} catch (CobolConversionException e) {
			assertEquals("BigDecimal value too large for target Cobol field. Host data at offset 0=0x000000000000000000", e.getMessage());
		}
		decimal = new BigDecimal("256897456");
		try {
			long offset = CobolZonedDecimalSimpleConverter.toHostSingle(
					decimal, 9, 9, 0, false, false, false, hostBytes, 0, "IBM01140");
			assertEquals(9, offset);
			assertEquals("f2f5f6f8f9f7f4f5f6", HostData.toHexString(hostBytes));
		} catch (CobolConversionException e) {
			fail(e.getMessage());
		}
	}

	public void testToHost456790p00675() throws HostException{
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
		assertEquals(11, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 11, 11, 5, false, false, true, hostBytes, 0, "IBM01147"));
		assertEquals("f4f5f6f7f9f0f0f0f6f7f5", HostData.toHexString(hostBytes));
	}
	public void testToHostM123456789012345p12 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[17];
   	
    	BigDecimal javaDecimal = new BigDecimal("-123456789012345.12");
		assertEquals(17, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 17, 17, 2, true, false, false, hostBytes, 0, "IBM01147"));
		assertEquals("f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f1d2", HostData.toHexString(hostBytes));
	}

	public void testToHostZero () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[17];
   	
    	BigDecimal javaDecimal = new BigDecimal("0");
		assertEquals(17, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 17, 17, 0, false, false, false, hostBytes, 0, "IBM01147"));
		assertEquals("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0", HostData.toHexString(hostBytes));
	}

    public void testToHostNull () throws HostException{
        // Create a host buffer
        byte[] hostBytes = new byte[17];
    
        BigDecimal javaDecimal = null;
        assertEquals(17, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 17, 17, 0, false, false, false, hostBytes, 0, "IBM01147"));
        assertEquals("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0", HostData.toHexString(hostBytes));
    }

    public void testToHostMZero () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[9];
   	
    	BigDecimal javaDecimal = new BigDecimal("-0");
		assertEquals(9, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 9, 9, 0, false, false, true, hostBytes, 0, "IBM01147"));
		assertEquals("f0f0f0f0f0f0f0f0f0", HostData.toHexString(hostBytes));
	}

	public void testToHostM67000p56007 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[11];
   	
    	BigDecimal javaDecimal = new BigDecimal("-67000.56007");
		assertEquals(11, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 11, 10, 5, true, true, true, hostBytes, 0, "IBM01147"));
		assertEquals("60f6f7f0f0f0f5f6f0f0f7", HostData.toHexString(hostBytes));
	}

	public void testToHostM67000p56007Trunc () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[10];
   	
    	BigDecimal javaDecimal = new BigDecimal("-67000.56007");
		assertEquals(10, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 10, 10, 2, true, false, false, hostBytes, 0, "IBM01147"));
		assertEquals("f0f0f0f6f7f0f0f0f5d6", HostData.toHexString(hostBytes));
	}

	public void testToHostSingleDigit () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[1];
   	
    	BigDecimal javaDecimal = new BigDecimal("1");
		assertEquals(1, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 1, 1, 0, false, false, false, hostBytes, 0, "IBM01147"));
		assertEquals("f1", HostData.toHexString(hostBytes));
	}

	public void testToHostp12345 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[6];
   	
    	BigDecimal javaDecimal = new BigDecimal(".12345");
		assertEquals(6, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 6, 6, 5, false, false, false, hostBytes, 0, "IBM01147"));
		assertEquals("f0f1f2f3f4f5", HostData.toHexString(hostBytes));
	}

	public void testToHostScaling () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[10];
   	
    	BigDecimal javaDecimal = new BigDecimal("1234567.89");
		assertEquals(10, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 10, 10, 3, false, false, false, hostBytes, 0, "IBM01147"));
		assertEquals("f1f2f3f4f5f6f7f8f9f0", HostData.toHexString(hostBytes));
	}

	public void testToHostPadding () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[5];
   	
    	BigDecimal javaDecimal = new BigDecimal("12");
		assertEquals(5, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 5, 5, 0, false, false, false, hostBytes, 0, "IBM01147"));
		assertEquals("f0f0f0f1f2", HostData.toHexString(hostBytes));
	}

	public void testToHostLarge () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[31];
   	
    	BigDecimal javaDecimal = new BigDecimal("1234567890123456789012345678901");
		assertEquals(31, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 31, 31, 0, false, false, false, hostBytes, 0, "IBM01147"));
		assertEquals("f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f6f7f8f9f0f1", HostData.toHexString(hostBytes));
	}

	public void testToHostSignSepLead () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[9];
   	
    	BigDecimal javaDecimal = new BigDecimal("-45.98754");
		assertEquals(9, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 9, 8, 6, true, true, true, hostBytes, 0, "IBM01147"));
		assertEquals("60f4f5f9f8f7f5f4f0", HostData.toHexString(hostBytes));
	}

	public void testToHostSignLead () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[8];
   	
    	BigDecimal javaDecimal = new BigDecimal("-45.98754");
		assertEquals(8, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 8, 8, 6, true, false, true, hostBytes, 0, "IBM01147"));
		assertEquals("d4f5f9f8f7f5f4f0", HostData.toHexString(hostBytes));
	}

	public void testToHostSignSepTrail () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[9];
   	
    	BigDecimal javaDecimal = new BigDecimal("-45.98754");
		assertEquals(9, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 9, 8, 6, true, true, false, hostBytes, 0, "IBM01147"));
		assertEquals("f4f5f9f8f7f5f4f060", HostData.toHexString(hostBytes));
	}

	public void testToHostSignTrail () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[8];
   	
    	BigDecimal javaDecimal = new BigDecimal("-45.98754");
		assertEquals(8, CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 8, 8, 6, true, false, false, hostBytes, 0, "IBM01147"));
		assertEquals("f4f5f9f8f7f5f4d0", HostData.toHexString(hostBytes));
	}

	public void testToHostInvalid () throws HostException{
    	try {
    		// Create a host buffer
    		byte[] hostBytes = new byte[1];
       	
        	BigDecimal javaDecimal = new BigDecimal("235.87");
    		CobolZonedDecimalSimpleConverter.toHostSingle(javaDecimal, 1, 1, 0, false, false, false, hostBytes, 0, "IBM01147");
			fail("BigDecimal too large not detected");
    	}  	catch (HostException he) {
    		assertEquals("BigDecimal value too large for target Cobol field. Host data at offset 0=0x00", he.getMessage());
    	}
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
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(11, 11, 5, false, false, true, hostBytes, 0, "IBM01147");
		assertEquals("456790.00675", javaDecimal.toString());
	}

	public void testFromHostM123456789012345p12 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f1d2");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(17, 17, 2, true, false, false, hostBytes, 0, "IBM01147");
		assertEquals("-123456789012345.12", javaDecimal.toString());
	}

	public void testFromHostZero () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(17, 17, 0, false, false, false, hostBytes, 0, "IBM01147");
		assertEquals("0", javaDecimal.toString());
	}

	public void testFromHostM67000p56007 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("60f6f7f0f0f0f5f6f0f0f7");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(11, 10, 5, true, true, true, hostBytes, 0, "IBM01147");
		assertEquals("-67000.56007", javaDecimal.toString());
	}

	public void testFromHostM67000p56007Trunc () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f0f0f0f6f7f0f0f0f5d6");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(10, 10, 2, true, false, false, hostBytes, 0, "IBM01147");
		assertEquals("-67000.56", javaDecimal.toString());
	}

	public void testFromHostSingleDigit () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f1");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(1, 1, 0, false, false, true, hostBytes, 0, "IBM01147");
		assertEquals("1", javaDecimal.toString());
	}

	public void testFromHostp12345 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f0f1f2f3f4f5");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(6, 6, 5, false, false, true, hostBytes, 0, "IBM01147");
		assertEquals("0.12345", javaDecimal.toString());
	}

	public void testFromHostScaling () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f1f2f3f4f5f6f7f8f9f0");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(10, 10, 3, false, false, true, hostBytes, 0, "IBM01147");
		assertEquals("1234567.890", javaDecimal.toString());
	}

	public void testFromHostPadding () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f0f0f0f1f2");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(5, 5, 0, false, false, true, hostBytes, 0, "IBM01147");
		assertEquals("12", javaDecimal.toString());
	}

	public void testFromHostLarge () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f6f7f8f9f0f1");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(31, 31, 0, false, false, true, hostBytes, 0, "IBM01147");
		assertEquals("1234567890123456789012345678901", javaDecimal.toString());
	}

	public void testFromSignSepLead () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("60f4f5f9f8f7f5f4f0");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(9, 8, 6, true, true, true, hostBytes, 0, "IBM01147");
		assertEquals("-45.987540", javaDecimal.toString());
	}

	public void testFromSignLead () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("d4f5f9f8f7f5f4f0");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(8, 8, 6, true, false, true, hostBytes, 0, "IBM01147");
		assertEquals("-45.987540", javaDecimal.toString());
	}

	public void testFromSignSepTrail () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f4f5f9f8f7f5f4f060");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(9, 8, 6, true, true, false, hostBytes, 0, "IBM01147");
		assertEquals("-45.987540", javaDecimal.toString());
	}

	public void testFromSignTrail () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f4f5f9f8f7f5f4d0");
   	
		BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(8, 8, 6, true, false, false, hostBytes, 0, "IBM01147");
		assertEquals("-45.987540", javaDecimal.toString());
	}
	public void testFromHostInvalid () throws HostException{
    	try {
			// Create a host buffer with host input
			byte[] hostBytes = HostData.toByteArray("1A");
	   	
			CobolZonedDecimalSimpleConverter.fromHostSingle(1, 1, 0, false, false, true, hostBytes, 0, "IBM01147");
			fail("Invalid zoned decimal not detected");
    	} 	catch (HostException he) {
    		assertEquals("Host data contains a byte that is not a valid zoned decimal byte. Host data at offset 0=0x1a", he.getMessage());
    	}
	}

	public void testFromHostInvalid2 () throws HostException{
    	try {
			// Create a host buffer with host input
			byte[] hostBytes = HostData.toByteArray("0A1f");
	   	
			CobolZonedDecimalSimpleConverter.fromHostSingle(2, 2, 0, false, false, true, hostBytes, 0, "IBM01147");
			fail("Invalid zoned decimal not detected");
    	}  	catch (HostException he) {
    		assertEquals("Host data contains a byte that is not a valid zoned decimal byte. Host data at offset 0=0x0a1f", he.getMessage());
    	}
	}

    /**
     * Case where there is not enough data left in the host buffer. The code should consider
     * that trailing nulls were omitted by the host.
     * @throws HostException if test fails
     */
    public void testToHostPartialData () throws HostException{
        // Create a host buffer with 6 bytes when 8 are necessary
        byte[] hostBytes = HostData.toByteArray("f4f5f9f8f760");
    
        BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(8, 8, 2, false, false, true, hostBytes, 0, "IBM01147");
        assertEquals("0.00",  javaDecimal.toString());
    }

    /**
     * Same as above but this time we are already past the offset.
     * @throws HostException if test fails
     */
    public void testToHostPartialDataPastOffset () throws HostException{
        // Create a host buffer with 6 bytes when 8 are necessary
        byte[] hostBytes = HostData.toByteArray("45679000675f");
    
        BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(8, 8, 2, false, false, true, hostBytes, 6, "IBM01147");
        assertEquals("0.00",  javaDecimal.toString());
    }
}
