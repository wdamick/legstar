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


public class BINNATUSTest extends CICWAbstractTest {
	private CIXSProgram m_program;

	protected void setUp() throws Exception {
		super.setUp();
        /** Create a program object based on properties file */
		m_program = new CIXSProgram("binnatus.properties");
		
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	/**
     * Request with values expected by binnatus
     */
    public void testBinNatUs() {
    	//                 9x4-9x4-9x4-9x4-";
    	String strP9X4  = "0000007F8001FFFF";
    	//                 9x8-----9x8-----9x8-----9x8-----";
    	String strP9X9  = "000000000000FFFE80000001FFFFFFFF";
    	//                 9x18------------9x18------------9x18------------9x18------------";
    	String strP9X18 = "000000000000000000000000FFFFFFFEFFFFFFFEFFFFFFFEFFFFFFFFFFFFFFFF";
    	String strRequest = strP9X4 + strP9X9 + strP9X18;
    	byte[] response = testProgram(m_program, strRequest, "BinNatUs Expected values", true);
    	assertEquals("0000007f8001ffff000000000000fffe80000001ffffffff000000000000000000000000fffffffefffffffefffffffeffffffffffffffff", toHexString(response).substring(0,112));
    }

}
