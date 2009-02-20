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

import java.io.UnsupportedEncodingException;

import com.legstar.coxb.convert.simple.CobolStringSimpleConverter;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

/**
 * Test the COBOL PIC X TYPE with conversion.
 *
 */
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

    public void testPadding() throws UnsupportedEncodingException {
        byte[] hostSource = HostData.toByteArray("c100c3c4");
        int i =  CobolStringSimpleConverter.pad(hostSource, 1, 3, "ISO-8859-1");
        assertEquals(2, i);
        assertEquals("c12020c4", HostData.toHexString(hostSource));

    }

    /**
     * Case where the host truncated that data so there are not enough bytes. Code
     * should pad with null bytes.
     * @throws HostException if test fails
     */
    public void testFromHostPartialData () throws HostException{
        // Create a host buffer
        byte[] hostSource = {-0x3F, -0x3E, -0x3D, -0x3C};

        String javaString = CobolStringSimpleConverter.fromHostSingle("IBM01140", 8, hostSource, 0);
        assertEquals("ABCD", javaString);
    }

    public void testFromHostPartialDataPastOffset () throws HostException{
        // Create a host buffer
        byte[] hostSource = {-0x3F, -0x3E, -0x3D, -0x3C};

        String javaString = CobolStringSimpleConverter.fromHostSingle("IBM01140", 4, hostSource, 4);
        assertTrue(null == javaString);
    }
}
