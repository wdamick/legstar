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



import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.typesmix.Dfhcommarea;

import junit.framework.TestCase;

public class UnmarshalTypesmixTest extends TestCase {

    public void testTypesmix() throws Exception {

        String hexString   = "c1c2c3c4c50041004200430044004500200020002000200e4040404040400f404040404040404040404040404040404040404040000000000000000000000000000000000000000000000000000000000cf0f0f0f0f0f0f0f0f0f0f0f0f0c0f040404040404040f0404040404040404040404040404040f0404040404040404040f04040404040404040404000000000000000000000000000000000000000004ef0f04bf0f0c54ef0f0000000000000";
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "typesmix");

        assertEquals("ABCDE",Dfhcommarea.getCAlphabetic());
        assertEquals("ABCDE    ",Dfhcommarea.getCNational());
        assertEquals("0e4040404040400f",HostData.toHexString(Dfhcommarea.getCDbcs()));
        assertEquals("",Dfhcommarea.getCAlphanumericEdited());
        assertEquals("",Dfhcommarea.getCAlphanumeric());
        byte[] cOctetString = {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
        assertEquals(HostData.toHexString(cOctetString),HostData.toHexString(Dfhcommarea.getCOctetString()));
        assertEquals("0.00",Dfhcommarea.getCPackedDecimal().toString());
        assertEquals("0",Dfhcommarea.getCNumericEdited1());
        assertEquals("0",Dfhcommarea.getCNumericEdited2());
        assertEquals("0",Dfhcommarea.getCNumericEdited3());
        assertEquals("0",Dfhcommarea.getCNumericEdited4());
        byte[] cIndex = {0x00,0x00,0x00,0x00};
        assertEquals(HostData.toHexString(cIndex),HostData.toHexString(Dfhcommarea.getCIndex()));
        byte[] cPointer = {0x00,0x00,0x00,0x00};
        assertEquals(HostData.toHexString(cPointer),HostData.toHexString(Dfhcommarea.getCPointer()));
        byte[] cProcPointer = {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
        assertEquals(HostData.toHexString(cProcPointer),HostData.toHexString(Dfhcommarea.getCProcPointer()));
        byte[] cFuncPointer = {0x00,0x00,0x00,0x00};
        assertEquals(HostData.toHexString(cFuncPointer),HostData.toHexString(Dfhcommarea.getCFuncPointer()));
    }
}
