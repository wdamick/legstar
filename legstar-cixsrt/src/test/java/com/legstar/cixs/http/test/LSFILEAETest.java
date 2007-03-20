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
package com.legstar.cixs.http.test;

import com.legstar.cixs.coxb.CIXSProgram;

public class LSFILEAETest extends CICWAbstractTest {
	
	private CIXSProgram m_program;
	
	protected void setUp() throws Exception {
		super.setUp();
        /** Create a program object based on properties file */
		m_program = new CIXSProgram("lsfileae.properties");
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	/**
     * Test running a simple host program
     */
    public void testValidProgram() {
    	byte[] response = testProgram(m_program, "f0f0f0f1f0f0", "Valid Program", true);
    	assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c", toHexString(response));
    }

    /**
     * Host program does not exist
     */
    public void testUnknownProgram() {
    	m_program.setProgramName("unknown");
        testProgram(m_program,"f0f0f0f1f0f0","Unknown Program", false);
    }
    
    /**
     * DataLength larger than request
     */
    public void testDataLengthLargerThanRequest() {
    	m_program.setDataLength(7);
        testProgram(m_program,"f0f0f0f1f0f0","Data length larger than request", false);
    }
 
    /**
     * DataLength larger than request
     */
    public void testDataLengthSmallerThanRequest() {
    	m_program.setDataLength(5);
        testProgram(m_program,"f0f0f0f1f0f0","Data length Smaller than request", true);
    }
    
    /**
     * Commarea length smaller than DataLength
     */
    public void testCommareaLengthSmallerThanDataLength() {
    	int commareaLength = m_program.getCommareaLength();
    	m_program.setDataLength( commareaLength + 1);
        testProgram(m_program,"f0f0f0f1f0f0","Commarea length smaller than Data length", false);
    }

    /**
     * Commarea length smaller than request
     */
    public void testCommareaLengthSmallerThanRequest() {
    	m_program.setCommareaLength(5);
        testProgram(m_program,"f0f0f0f1f0f0","Commarea length smaller than request", false);
    }


}
