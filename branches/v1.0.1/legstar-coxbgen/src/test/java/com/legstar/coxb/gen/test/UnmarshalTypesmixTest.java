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
package com.legstar.coxb.gen.test;


import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolUnmarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;

public class UnmarshalTypesmixTest extends TestCase {

	public void testTypesmix() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		String hexString   = "c1c2c3c4c50041004200430044004500200020002000200e4040404040400f404040404040404040404040404040404040404040000000000000000000000000000000000000000000000000000000000cf0f0f0f0f0f0f0f0f0f0f0f0f0c0f040404040404040f0404040404040404040404040404040f0404040404040404040f04040404040404040404000000000000000000000000000000000000000004ef0f04bf0f0c54ef0f0000000000000";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.typesmix.ObjectFactory objectFactory = new com.legstar.test.coxb.typesmix.ObjectFactory();

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.typesmix.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.typesmix.bind.DfhcommareaTypeBinding(objectFactory);
		ccem.accept(uv);
		com.legstar.test.coxb.typesmix.DfhcommareaType dfhcommareaType = ccem.getJaxbObject();
		
		assertEquals("ABCDE",dfhcommareaType.getCAlphabetic());
		assertEquals("ABCDE    ",dfhcommareaType.getCNational());
		assertEquals("0e4040404040400f",HostData.toHexString(dfhcommareaType.getCDbcs()));
		assertEquals("              ",dfhcommareaType.getCAlphanumericEdited());
		assertEquals("       ",dfhcommareaType.getCAlphanumeric());
		byte[] cOctetString = {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
		assertEquals(HostData.toHexString(cOctetString),HostData.toHexString(dfhcommareaType.getCOctetString()));
		assertEquals("0.00",dfhcommareaType.getCPackedDecimal().toString());
		assertEquals("0       ",dfhcommareaType.getCNumericEdited1());
		assertEquals("0               ",dfhcommareaType.getCNumericEdited2());
		assertEquals("0         ",dfhcommareaType.getCNumericEdited3());
		assertEquals("0          ",dfhcommareaType.getCNumericEdited4());
		byte[] cIndex = {0x00,0x00,0x00,0x00};
		assertEquals(HostData.toHexString(cIndex),HostData.toHexString(dfhcommareaType.getCIndex()));
		byte[] cPointer = {0x00,0x00,0x00,0x00};
		assertEquals(HostData.toHexString(cPointer),HostData.toHexString(dfhcommareaType.getCPointer()));
		byte[] cProcPointer = {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
		assertEquals(HostData.toHexString(cProcPointer),HostData.toHexString(dfhcommareaType.getCProcPointer()));
		byte[] cFuncPointer = {0x00,0x00,0x00,0x00};
		assertEquals(HostData.toHexString(cFuncPointer),HostData.toHexString(dfhcommareaType.getCFuncPointer()));
	}
}
