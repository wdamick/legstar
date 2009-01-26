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

import com.legstar.coxb.convert.simple.CobolBinarySimpleConverter;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import java.math.BigDecimal;
import junit.framework.TestCase;

public class BinaryTest extends TestCase {

	public void testToHost1234p00 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[4];
   	
    	BigDecimal javaDecimal = new BigDecimal("1234.00");
		assertEquals(4, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, false, hostBytes, 0));
		assertEquals("0001e208", HostData.toHexString(hostBytes));
	}

	public void testFromHost1234p00 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("0001e208");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(4, false, 8, 2, hostBytes, 0);
		assertEquals("1234.00", javaDecimal.toString());
	}

	public void testToHostM1234p00 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[4];
   	
    	BigDecimal javaDecimal = new BigDecimal("-1234.00");
		assertEquals(4, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, true, hostBytes, 0));
		assertEquals("fffe1df8", HostData.toHexString(hostBytes));
	}

	public void testFromHostM1234p00 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("fffe1df8");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(4, true, 8, 2, hostBytes, 0);
		assertEquals("-1234.00", javaDecimal.toString());
	}

	public void testToHostWriteOverflow () throws HostException {
    	try {
			// Create a host buffer that is clearly too small
			byte[] hostBytes = new byte[2];
	   	
	    	BigDecimal javaDecimal = new BigDecimal("123456789012");
	    	CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, false, hostBytes, 0);
    		fail("overflow not detected");
    	}	catch (HostException he) {
    		assertEquals("Attempt to write past end of host source buffer. Host data at offset 0=0x0000",he.getMessage());
    	}
	}
	
	/**
	 * Case where there is not enough data left in the host buffer. The code should consider
	 * that trailing nulls were omitted by the host.
	 * @throws HostException if test fails
	 */
	public void testToHostPartialData () throws HostException{
		// Create a host buffer with 4 bytes when 8 are necessary
		byte[] hostBytes = HostData.toByteArray("fffe1df8");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(8, true, 8, 2, hostBytes, 0);
		assertEquals("-5299989643264.00",  javaDecimal.toString());
	}

    /**
     * Same as above but this time we are already past the offset.
     * @throws HostException if test fails
     */
    public void testToHostPartialDataPastOffset () throws HostException{
        // Create a host buffer with 4 bytes when 8 are necessary
        byte[] hostBytes = HostData.toByteArray("fffe1df8");
    
        BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(8, true, 8, 2, hostBytes, 5);
        assertEquals("0.00",  javaDecimal.toString());
    }

    public void testToHostJavaTooLarge () throws HostException{
    	try {
			// Create a host buffer
			byte[] hostBytes = new byte[4];
	   	
	    	BigDecimal javaDecimal = new BigDecimal("4294967296");
	    	CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, false, hostBytes, 0);
    		fail("overflow not detected");
    	}   	catch (HostException he) {
    		assertEquals("Java binary too large for Cobol element. Host data at offset 0=0x00000000",he.getMessage());
    	}
	}

	public void testToHostJavaMax () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[4];
   	
    	BigDecimal javaDecimal = new BigDecimal("4294967295");
		assertEquals(4, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, false, hostBytes, 0));
		assertEquals("ffffffff", HostData.toHexString(hostBytes));
	}

	public void testToHostJavaMaxM1 () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[4];
   	
    	BigDecimal javaDecimal = new BigDecimal("4294967294");
		assertEquals(4, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, false, hostBytes, 0));
		assertEquals("fffffffe", HostData.toHexString(hostBytes));
	}

	public void testToHostJavaFill () throws HostException{
		// Create a host buffer
		byte[] hostBytes = new byte[4];
   	
    	BigDecimal javaDecimal = new BigDecimal("7");
		assertEquals(4, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, false, hostBytes, 0));
		assertEquals("00000007", HostData.toHexString(hostBytes));
	}
	
	public void testToHostZero () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[2];
   	
    	BigDecimal javaDecimal = new BigDecimal("0");
		assertEquals(2, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 2, false, hostBytes, 0));
		assertEquals("0000", HostData.toHexString(hostBytes));
	}

	public void testFromZero () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("0000");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(2, false, 4, 0, hostBytes, 0);
		assertEquals("0", javaDecimal.toString());
	}
	
	public void testToHost74 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[2];
   	
    	BigDecimal javaDecimal = new BigDecimal("74");
		assertEquals(2, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 2, false, hostBytes, 0));
		assertEquals("004a", HostData.toHexString(hostBytes));
	}

	public void testFrom74 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("004a");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(2, false, 4, 0, hostBytes, 0);
		assertEquals("74", javaDecimal.toString());
	}

	public void testToHost127 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[2];
   	
    	BigDecimal javaDecimal = new BigDecimal("127");
		assertEquals(2, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 2, false, hostBytes, 0));
		assertEquals("007f", HostData.toHexString(hostBytes));
	}

	public void testFrom127 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("007f");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(2, false, 4, 0, hostBytes, 0);
		assertEquals("127", javaDecimal.toString());
	}

	public void testToHost32769 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[2];
   	
    	BigDecimal javaDecimal = new BigDecimal("32769");
		assertEquals(2, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 2, false, hostBytes, 0));
		assertEquals("8001", HostData.toHexString(hostBytes));
	}

	public void testFrom32769 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("8001");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(2, false, 4, 0, hostBytes, 0);
		assertEquals("32769", javaDecimal.toString());
	}

	public void testToHost65535 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[2];
   	
    	BigDecimal javaDecimal = new BigDecimal("65535");
		assertEquals(2, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 2, false, hostBytes, 0));
		assertEquals("ffff", HostData.toHexString(hostBytes));
	}

	public void testFrom65535 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("ffff");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(2, false, 4, 0, hostBytes, 0);
		assertEquals("65535", javaDecimal.toString());
	}
	public void testToHostM32768 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[2];
   	
    	BigDecimal javaDecimal = new BigDecimal("-32768");
		assertEquals(2, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 2, true, hostBytes, 0));
		assertEquals("8000", HostData.toHexString(hostBytes));
	}

	public void testFromM32768 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("8000");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(2, true, 4, 0, hostBytes, 0);
		assertEquals("-32768", javaDecimal.toString());
	}
	public void testToHostM128 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[2];
   	
    	BigDecimal javaDecimal = new BigDecimal("-128");
		assertEquals(2, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 2, true, hostBytes, 0));
		assertEquals("ff80", HostData.toHexString(hostBytes));
	}

	public void testFromM128 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("ff80");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(2, true, 4, 0, hostBytes, 0);
		assertEquals("-128", javaDecimal.toString());
	}
	public void testToHostM75 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[2];
   	
    	BigDecimal javaDecimal = new BigDecimal("-75");
		assertEquals(2, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 2, true, hostBytes, 0));
		assertEquals("ffb5", HostData.toHexString(hostBytes));
	}

	public void testFromM75 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("ffb5");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(2, true, 4, 0, hostBytes, 0);
		assertEquals("-75", javaDecimal.toString());
	}

	public void testToHost1045 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[2];
   	
    	BigDecimal javaDecimal = new BigDecimal("1045");
		assertEquals(2, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 2, false, hostBytes, 0));
		assertEquals("0415", HostData.toHexString(hostBytes));
	}

	public void testFrom1045 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("0415");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(2, false, 4, 0, hostBytes, 0);
		assertEquals("1045", javaDecimal.toString());
	}

	public void testToHost32767 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[2];
   	
    	BigDecimal javaDecimal = new BigDecimal("32767");
		assertEquals(2, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 2, true, hostBytes, 0));
		assertEquals("7fff", HostData.toHexString(hostBytes));
	}

	public void testFrom32767 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("7fff");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(2, true, 4, 0, hostBytes, 0);
		assertEquals("32767", javaDecimal.toString());
	}

	public void testToHost4BytesZero () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[4];
   	
    	BigDecimal javaDecimal = new BigDecimal("0");
		assertEquals(4, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, false, hostBytes, 0));
		assertEquals("00000000", HostData.toHexString(hostBytes));
	}

	public void testFrom4BytesZero () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("00000000");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(4, false, 8, 0, hostBytes, 0);
		assertEquals("0", javaDecimal.toString());
	}

	public void testToHost4Bytes74 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[4];
   	
    	BigDecimal javaDecimal = new BigDecimal("74");
		assertEquals(4, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, false, hostBytes, 0));
		assertEquals("0000004a", HostData.toHexString(hostBytes));
	}

	public void testFrom4Bytes74 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("0000004a");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(4, false, 8, 0, hostBytes, 0);
		assertEquals("74", javaDecimal.toString());
	}

	public void testToHost4Bytes65534 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[4];
   	
    	BigDecimal javaDecimal = new BigDecimal("65534");
		assertEquals(4, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, false, hostBytes, 0));
		assertEquals("0000fffe", HostData.toHexString(hostBytes));
	}

	public void testFrom4Bytes65534 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("0000fffe");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(4, false, 8, 0, hostBytes, 0);
		assertEquals("65534", javaDecimal.toString());
	}

	public void testToHost4Bytes2147483649 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[4];
   	
    	BigDecimal javaDecimal = new BigDecimal("2147483649");
		assertEquals(4, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, false, hostBytes, 0));
		assertEquals("80000001", HostData.toHexString(hostBytes));
	}

	public void testFrom4Bytes2147483649 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("80000001");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(4, false, 8, 0, hostBytes, 0);
		assertEquals("2147483649", javaDecimal.toString());
	}
	
	public void testToHost4Bytes4294967295 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[4];
   	
    	BigDecimal javaDecimal = new BigDecimal("4294967295");
		assertEquals(4, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, false, hostBytes, 0));
		assertEquals("ffffffff", HostData.toHexString(hostBytes));
	}

	public void testFrom4Bytes4294967295 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("ffffffff");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(4, false, 8, 0, hostBytes, 0);
		assertEquals("4294967295", javaDecimal.toString());
	}

	public void testToHost4BytesM2147483648 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[4];
   	
    	BigDecimal javaDecimal = new BigDecimal("-2147483648");
		assertEquals(4, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, true, hostBytes, 0));
		assertEquals("80000000", HostData.toHexString(hostBytes));
	}

	public void testFrom4BytesM2147483648 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("80000000");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(4, true, 8, 0, hostBytes, 0);
		assertEquals("-2147483648", javaDecimal.toString());
	}

	public void testToHost4BytesM75 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[4];
   	
    	BigDecimal javaDecimal = new BigDecimal("-75");
		assertEquals(4, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, true, hostBytes, 0));
		assertEquals("ffffffb5", HostData.toHexString(hostBytes));
	}

	public void testFrom4BytesM75 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("ffffffb5");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(4, true, 8, 0, hostBytes, 0);
		assertEquals("-75", javaDecimal.toString());
	}

	public void testToHost4BytesM128 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[4];
   	
    	BigDecimal javaDecimal = new BigDecimal("-128");
		assertEquals(4, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, true, hostBytes, 0));
		assertEquals("ffffff80", HostData.toHexString(hostBytes));
	}

	public void testFrom4BytesM128 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("ffffff80");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(4, true, 8, 0, hostBytes, 0);
		assertEquals("-128", javaDecimal.toString());
	}

	public void testToHost4Bytes123456789 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[4];
   	
    	BigDecimal javaDecimal = new BigDecimal("123456789");
		assertEquals(4, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, true, hostBytes, 0));
		assertEquals("075bcd15", HostData.toHexString(hostBytes));
	}

	public void testFrom4Bytes123456789 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("075bcd15");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(4, true, 8, 0, hostBytes, 0);
		assertEquals("123456789", javaDecimal.toString());
	}

	public void testToHost4Bytes2147483647 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[4];
   	
    	BigDecimal javaDecimal = new BigDecimal("2147483647");
		assertEquals(4, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, true, hostBytes, 0));
		assertEquals("7fffffff", HostData.toHexString(hostBytes));
	}

	public void testFrom4Bytes2147483647 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("7fffffff");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(4, true, 8, 0, hostBytes, 0);
		assertEquals("2147483647", javaDecimal.toString());
	}

	public void testToHost8BytesZero () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[8];
   	
    	BigDecimal javaDecimal = new BigDecimal("0");
		assertEquals(8, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 8, false, hostBytes, 0));
		assertEquals("0000000000000000", HostData.toHexString(hostBytes));
	}

	public void testFrom8BytesZero () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("0000000000000000");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(8, false, 18, 0, hostBytes, 0);
		assertEquals("0", javaDecimal.toString());
	}

	public void testToHost8Bytes74 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[8];
   	
    	BigDecimal javaDecimal = new BigDecimal("74");
		assertEquals(8, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 8, false, hostBytes, 0));
		assertEquals("000000000000004a", HostData.toHexString(hostBytes));
	}

	public void testFrom8Bytes74 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("000000000000004a");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(8, false, 18, 0, hostBytes, 0);
		assertEquals("74", javaDecimal.toString());
	}

	public void testToHost8Bytes4294967294 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[8];
   	
    	BigDecimal javaDecimal = new BigDecimal("4294967294");
		assertEquals(8, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 8, false, hostBytes, 0));
		assertEquals("00000000fffffffe", HostData.toHexString(hostBytes));
	}

	public void testFrom8Bytes4294967294 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("00000000fffffffe");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(8, false, 18, 0, hostBytes, 0);
		assertEquals("4294967294", javaDecimal.toString());
	}

	public void testToHost8Bytes18446744069414584318 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[8];
   	
    	BigDecimal javaDecimal = new BigDecimal("18446744069414584318");
		assertEquals(8, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 8, false, hostBytes, 0));
		assertEquals("fffffffefffffffe", HostData.toHexString(hostBytes));
	}

	public void testFrom8Bytes18446744069414584318 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("fffffffefffffffe");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(8, false, 18, 0, hostBytes, 0);
		assertEquals("18446744069414584318", javaDecimal.toString());
	}

	public void testToHost8Bytes18446744073709551615 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[8];
   	
    	BigDecimal javaDecimal = new BigDecimal("18446744073709551615");
		assertEquals(8, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 8, false, hostBytes, 0));
		assertEquals("ffffffffffffffff", HostData.toHexString(hostBytes));
	}

	public void testFrom8Bytes18446744073709551615 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("ffffffffffffffff");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(8, false, 18, 0, hostBytes, 0);
		assertEquals("18446744073709551615", javaDecimal.toString());
	}
	
	public void testToHost8BytesM9223372036854775808 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[8];
   	
    	BigDecimal javaDecimal = new BigDecimal("-9223372036854775808");
		assertEquals(8, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 8, true, hostBytes, 0));
		assertEquals("8000000000000000", HostData.toHexString(hostBytes));
	}

	public void testFrom8BytesM9223372036854775808 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("8000000000000000");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(8, true, 18, 0, hostBytes, 0);
		assertEquals("-9223372036854775808", javaDecimal.toString());
	}

	public void testToHost8BytesM75 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[8];
   	
    	BigDecimal javaDecimal = new BigDecimal("-75");
		assertEquals(8, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 8, true, hostBytes, 0));
		assertEquals("ffffffffffffffb5", HostData.toHexString(hostBytes));
	}

	public void testFrom8BytesM75 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("ffffffffffffffb5");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(8, true, 18, 0, hostBytes, 0);
		assertEquals("-75", javaDecimal.toString());
	}

	public void testToHost8BytesM4294967294 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[8];
   	
    	BigDecimal javaDecimal = new BigDecimal("-4294967294");
		assertEquals(8, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 8, true, hostBytes, 0));
		assertEquals("ffffffff00000002", HostData.toHexString(hostBytes));
	}

	public void testFrom8BytesM4294967294 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("ffffffff00000002");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(8, true, 18, 0, hostBytes, 0);
		assertEquals("-4294967294", javaDecimal.toString());
	}
	
	public void testToHost8Bytes17179869183 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[8];
   	
    	BigDecimal javaDecimal = new BigDecimal("17179869183");
		assertEquals(8, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 8, true, hostBytes, 0));
		assertEquals("00000003ffffffff", HostData.toHexString(hostBytes));
	}

	public void testFrom8Bytes17179869183 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("00000003ffffffff");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(8, true, 18, 0, hostBytes, 0);
		assertEquals("17179869183", javaDecimal.toString());
	}

	public void testToHost8Bytes9223372036854775807 () throws HostException{
		// Create a host buffer to receive output
		byte[] hostBytes = new byte[8];
   	
    	BigDecimal javaDecimal = new BigDecimal("9223372036854775807");
		assertEquals(8, CobolBinarySimpleConverter.toHostSingle(javaDecimal, 8, true, hostBytes, 0));
		assertEquals("7fffffffffffffff", HostData.toHexString(hostBytes));
	}

	public void testFrom8Bytes9223372036854775807 () throws HostException{
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("7fffffffffffffff");
   	
		BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(8, true, 18, 0, hostBytes, 0);
		assertEquals("9223372036854775807", javaDecimal.toString());
	}

}
