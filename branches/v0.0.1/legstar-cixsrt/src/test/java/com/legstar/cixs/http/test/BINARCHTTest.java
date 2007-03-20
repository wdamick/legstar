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


public class BINARCHTTest extends CICWAbstractTest {
	private CIXSProgram m_program;

	protected void setUp() throws Exception {
		super.setUp();
        /** Create a program object based on properties file */
		m_program = new CIXSProgram("binarcht.properties");
		
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	/**
     * Request with random mix of binaries
     */
    public void testRandomMix() {
    	//               9x4-9x4-9x8-----9x8-----9x18------------9x18------------";
    	String strP9  = "0000FFFF00000000FFFFFFFF0000000000000000FFFFFFFFFFFFFFFF";
    	//               S9x4S9x4S9x8----S9x8----S9x18-----------S9x18-----------";
    	String strPS9 = "80007FFF800000007FFFFFFF80000000000000007FFFFFFFFFFFFFFF";
    	String strRequest = strP9 + strPS9;
    	byte[] response = testProgram(m_program, strRequest, "Random mix of binaries", true);
    	assertEquals("0000ffff00000000499602d2000000000000000001b69b4ba630f34e80007ffff8a432eb075bcd15ffd423aba294b479002bdc545d6b4b87", toHexString(response).substring(0,112));
    }

}
