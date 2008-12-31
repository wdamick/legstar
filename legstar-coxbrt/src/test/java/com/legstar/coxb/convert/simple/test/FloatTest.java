/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.convert.simple.test;

import com.legstar.coxb.convert.simple.CobolFloatSimpleConverter;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

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

    /**
     * Case where we are past the offset. Not enough data sent from the mainframe.
     * @throws HostException if test fails
     */
    public void testFromHostPastOffset () throws HostException{
        // Create a host buffer
        byte[] hostSource = {0x36, 0x16};
    
        Float javaFloat = CobolFloatSimpleConverter.fromHostSingle(4, hostSource, 0);
        assertEquals("0.0", javaFloat.toString());
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
