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

import com.legstar.coxb.convert.simple.CobolStringSimpleConverter;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;

public class StringTest extends TestCase {

	public void testToHostUnsupportedCharset (){
    	try {
        	String unknownCharSet = "IBM0114Q"; // An unavailable charset
    		// Create a host buffer
    		byte[] hostBytes = new byte[4];
       	
        	String javaString = "ABCD";
    		CobolStringSimpleConverter.toHostSingle(javaString,unknownCharSet,4,false, hostBytes, 0);
    		fail("charset not tested correctly");
    	}    	catch (HostException he) {
    		assertEquals("UnsupportedEncodingException:IBM0114Q",he.getMessage());
    	}
	}

	public void testToHostABCD () throws HostException{
    	String usCharSet = "IBM01140"; // Default US charset
		// Create a host buffer
		byte[] hostBytes = new byte[4];
   	
    	String javaString = "ABCD";
		assertEquals(4, CobolStringSimpleConverter.toHostSingle(javaString,usCharSet,4,false, hostBytes, 0));
		assertEquals("c1c2c3c4", HostData.toHexString(hostBytes));
	}

	public void testFromHostABCD () throws HostException{
    	String usCharSet = "IBM01140"; // Default US charset
		// Create a host buffer
		byte[] hostSource = {-0x3F, -0x3E, -0x3D, -0x3C};
   	
    	String javaString = CobolStringSimpleConverter.fromHostSingle(usCharSet,4, hostSource, 0);
		assertEquals("ABCD", javaString.toString());
	}

	public void testToHostJustified () throws HostException{
    	String usCharSet = "IBM01140"; // Default US charset
		// Create a host buffer
		byte[] hostBytes = new byte[6];
   	
    	String javaString = "ABCD";
		/* Right justify */
    	assertEquals(6, CobolStringSimpleConverter.toHostSingle(javaString,usCharSet,6,true, hostBytes, 0));
		assertEquals("4040c1c2c3c4", HostData.toHexString(hostBytes));

		/* Left justify */
    	assertEquals(6, CobolStringSimpleConverter.toHostSingle(javaString,usCharSet,6,false, hostBytes, 0));
		assertEquals("c1c2c3c44040", HostData.toHexString(hostBytes));
	}

	public void testToHostTruncate () throws HostException{
    	String usCharSet = "IBM01140"; // Default US charset
		// Create a host buffer
		byte[] hostBytes = new byte[4];
   	
    	String javaString = "ABCDEFG";
    	assertEquals(4, CobolStringSimpleConverter.toHostSingle(javaString,usCharSet,4,false, hostBytes, 0));
		assertEquals("c1c2c3c4", HostData.toHexString(hostBytes));
	}

	public void testToHostWriteOverflow () throws HostException{
    	try {
        	String usCharSet = "IBM01140"; // Default US charset
			// Create a host buffer
			byte[] hostBytes = new byte[4];
	   	
	    	String javaString = "ABCDEFG";
	    	CobolStringSimpleConverter.toHostSingle(javaString,usCharSet,6,false, hostBytes, 0);
    		fail("overflow not detected");
    	}    	catch (HostException he) {
    		assertEquals("Attempt to write past end of host source buffer. Host data at offset 0=0x00000000",he.getMessage());
    	}
	}

	public void testToHostReadOverflow () throws HostException{
    	try {
        	String usCharSet = "IBM01140"; // Default US charset
    		// Create a host buffer
    		byte[] hostSource = {-0x3F, -0x3E, -0x3D, -0x3C};
       	
        	CobolStringSimpleConverter.fromHostSingle(usCharSet,6, hostSource, 0);
    		fail("overflow not detected");
    	}    	catch (HostException he) {
    		assertEquals("Attempt to read past end of host source buffer. Host data at offset 0=0xc1c2c3c4",he.getMessage());
    	}
	}
	public void testToHostCharset () throws HostException{
    	String frenchCharSet = "IBM01147"; // French charset
		// Create a host buffer
		byte[] hostBytes = new byte[43];
   	
    	String javaString = "ça c'est un problème élémentaire à résoudre";
		assertEquals(43, CobolStringSimpleConverter.toHostSingle(javaString,frenchCharSet,43,false, hostBytes, 0));
		assertEquals("e08140837d85a2a340a495409799968293d0948540c093c0948595a381899985407c4099c0a296a4849985", HostData.toHexString(hostBytes));
		
		/* Switch to US charset to see the difference*/
    	String usCharSet = "IBM01140"; // Default US charset
		assertEquals(43, CobolStringSimpleConverter.toHostSingle(javaString,usCharSet,43,false, hostBytes, 0));
		assertEquals("488140837d85a2a340a49540979996829354948540519351948595a3818999854044409951a296a4849985", HostData.toHexString(hostBytes));
	}

	public void testFromHostCharset () throws HostException{
    	String frenchCharSet = "IBM01147"; // French charset
		// Create a host buffer
		byte[] hostSource = HostData.toByteArray("e08140837d85a2a340a495409799968293d0948540c093c0948595a381899985407c4099c0a296a4849985");
   	
    	String javaString = CobolStringSimpleConverter.fromHostSingle(frenchCharSet,43, hostSource, 0);
		assertEquals("ça c'est un problème élémentaire à résoudre", javaString.toString());
		
		/* Switch to US charset to see the difference*/
    	String usCharSet = "IBM01140"; // Default US charset
    	String javaString2 = CobolStringSimpleConverter.fromHostSingle(usCharSet,43, hostSource, 0);
		assertEquals("\\a c'est un probl}me {l{mentaire @ r{soudre", javaString2.toString());
	}
	
	public void testFromHostLowValueReplacement () throws HostException{
    	String usCharSet = "IBM01140"; // Default US charset
		// Create a host buffer
		byte[] hostSource = HostData.toByteArray("c100c3c4");
   	
    	String javaString = CobolStringSimpleConverter.fromHostSingle(usCharSet,4, hostSource, 0);
		assertEquals("A CD", javaString.toString());
	}

}
