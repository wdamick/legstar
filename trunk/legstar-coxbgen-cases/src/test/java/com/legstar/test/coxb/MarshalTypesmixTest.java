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
package com.legstar.test.coxb;

import java.math.BigDecimal;

import com.legstar.test.coxb.typesmix.DfhcommareaType;

import junit.framework.TestCase;

public class MarshalTypesmixTest extends TestCase {

	private final static String SCHEMA_NAME = "typesmix";
	
	public void testTypesmix() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		
		dfhcommareaType.setCAlphabetic("ABCDE");
		dfhcommareaType.setCNational("ABCDE");
		byte[] cCDbcs = {0x0E,0x40,0x40,0x40,0x40,0x40,0x40, 0x0F};
		dfhcommareaType.setCDbcs(cCDbcs);
		dfhcommareaType.setCAlphanumericEdited("");
		dfhcommareaType.setCAlphanumeric("");
		byte[] cOctetString = {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
		dfhcommareaType.setCOctetString(cOctetString);
		dfhcommareaType.setCPackedDecimal(new BigDecimal(0));
		dfhcommareaType.setCNumericEdited1("0");
		dfhcommareaType.setCNumericEdited2("0");
		dfhcommareaType.setCNumericEdited3("0");
		dfhcommareaType.setCNumericEdited4("0");
		dfhcommareaType.setCExternalFloating("+00.00E+00");
		byte[] cIndex = {0x00,0x00,0x00,0x00};
		dfhcommareaType.setCIndex(cIndex);
		byte[] cPointer = {0x00,0x00,0x00,0x00};
		dfhcommareaType.setCPointer(cPointer);
		byte[] cProcPointer = {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
		dfhcommareaType.setCProcPointer(cProcPointer);
		byte[] cFuncPointer = {0x00,0x00,0x00,0x00};
		dfhcommareaType.setCFuncPointer(cFuncPointer);

		assertEquals("c1c2c3c4c50041004200430044004500200020002000200e4040404040400f404040404040404040404040404040404040404040000000000000000000000000000000000000000000000000000000000cf0f0f0f0f0f0f0f0f0f0f0f0f0c0f040404040404040f0404040404040404040404040404040f0404040404040404040f04040404040404040404000000000000000000000000000000000000000004ef0f04bf0f0c54ef0f0000000000000",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 176));
	}
}
