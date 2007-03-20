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


public class BINNATSITest extends CICWAbstractTest {
	private CIXSProgram m_program;

	protected void setUp() throws Exception {
		super.setUp();
        /** Create a program object based on properties file */
		m_program = new CIXSProgram("binnatsi.properties");
		
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	/**
     * Request with values expected by binnatsi
     */
    public void testBinNatSi() {
    	//                 S9x4S9x4S9x4S9x4-";
    	String strP9X4  = "8000FF8004157FFF";
    	//                 S9x8----S9x8----S9x8----S9x8----";
    	String strP9X9  = "80000000FFFFFF80075BCD157FFFFFFF";
    	//                 S9x18-----------S9x18-----------S9x18-----------S9x18-----------";
    	String strP9X18 = "8000000000000000FFFFFFFF0000000200000003FFFFFFFF7FFFFFFFFFFFFFFF";
    	String strRequest = strP9X4 + strP9X9 + strP9X18;
    	byte[] response = testProgram(m_program, strRequest, "BinNatSi Expected values", true);
    	assertEquals("8000ff8004157fff80000000ffffff80075bcd157fffffff8000000000000000ffffffff0000000200000003ffffffff7fffffffffffffff", toHexString(response).substring(0,112));
    }

}
