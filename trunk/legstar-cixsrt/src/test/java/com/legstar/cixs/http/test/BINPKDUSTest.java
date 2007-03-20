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


public class BINPKDUSTest extends CICWAbstractTest {
	private CIXSProgram m_program;

	protected void setUp() throws Exception {
		super.setUp();
        /** Create a program object based on properties file */
		m_program = new CIXSProgram("binpkdus.properties");
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	/**
     * Request with values expected by binnatsi
     */
    public void testBinNatSi() {
    	//                   X1X1X2--X7------X18-----------------";
    	String strCompat  = "0F3F012F0032769F0123456789012345678F";
    	//                   X19-----------------X31-----------------------------";
    	String strExtend  = "1234567890123456789F1234567890123456789012345678901F";
    	//                 S9x18-----------S9x18-----------S9x18-----------S9x18-----------";
    	String strRequest = strCompat + strExtend;
    	byte[] response = testProgram(m_program, strRequest, "BinNatSi Expected values", true);
    	assertEquals("0f3f012f0032769f0123456789012345678f1234567890123456789f1234567890123456789012345678901f", toHexString(response).substring(0,88));
    }

}
