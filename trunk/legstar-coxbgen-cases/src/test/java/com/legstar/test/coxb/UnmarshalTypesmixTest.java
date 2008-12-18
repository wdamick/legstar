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

/**
 * Unmarshal typesmix.
 *
 */
public class UnmarshalTypesmixTest extends TestCase {

    /**
     * Unmarshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testTypesmix() throws Exception {

        String hexString   = "c1c2c3c4c5"
            + "004100420043004400450020002000200020"
            + "0e4040404040400f"
            + "4040404040404040404040404040"
            + "40404040404040"
            + "0000000000000000"
            + "00000000000000000000000000000000000000000c"
            + "f0f0f0f0f0f0f0f0f0f0f0f0f0c0f0"
            + "40404040404040f0404040404040404040404040404040f0"
            + "404040404040404040f040404040404040404040"
            + "00000000000000000000000000000000000000004ef0f04bf0f0c54ef0f0"
            + "00000000"
            + "0000";
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "typesmix");

        assertEquals("ABCDE", dfhcommarea.getCAlphabetic());
        assertEquals("ABCDE    ", dfhcommarea.getCNational());
        assertEquals("0e4040404040400f", HostData.toHexString(dfhcommarea.getCDbcs()));
        assertEquals("", dfhcommarea.getCAlphanumericEdited());
        assertEquals("", dfhcommarea.getCAlphanumeric());
        byte[] cOctetString = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
        assertEquals(HostData.toHexString(cOctetString), HostData.toHexString(dfhcommarea.getCOctetString()));
        assertEquals("0.00", dfhcommarea.getCPackedDecimal().toString());
        assertEquals("0", dfhcommarea.getCNumericEdited1());
        assertEquals("0", dfhcommarea.getCNumericEdited2());
        assertEquals("0", dfhcommarea.getCNumericEdited3());
        assertEquals("0", dfhcommarea.getCNumericEdited4());
        byte[] cIndex = {0x00, 0x00, 0x00, 0x00};
        assertEquals(HostData.toHexString(cIndex), HostData.toHexString(dfhcommarea.getCIndex()));
        byte[] cPointer = {0x00, 0x00, 0x00, 0x00};
        assertEquals(HostData.toHexString(cPointer), HostData.toHexString(dfhcommarea.getCPointer()));
        byte[] cProcPointer = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
        assertEquals(HostData.toHexString(cProcPointer), HostData.toHexString(dfhcommarea.getCProcPointer()));
        byte[] cFuncPointer = {0x00, 0x00, 0x00, 0x00};
        assertEquals(HostData.toHexString(cFuncPointer), HostData.toHexString(dfhcommarea.getCFuncPointer()));
    }
}
