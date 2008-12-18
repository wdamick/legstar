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
import java.math.BigInteger;

import com.legstar.coxb.host.HostData;

import junit.framework.TestCase;
import com.legstar.test.coxb.alltypes.Dfhcommarea;

/**
 * Unmarshal alltypes.
 *
 */
public class UnmarshalAlltypesTest extends TestCase {

    /**
     * Unmarshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testAlltypes() throws Exception {

        String hexString   = "c1c2c3c4"
            + "01020000"
            + "fc5c"
            + "000f"
            + "0001343a"
            + "000001c4"
            + "0000000000004532456d"
            + "0000000000007800056f"
            + "0000000000000000087554907654321c"
            + "0000000000000000000564678008321f"
            + "000007545f"
            + "45543ae9"
            + "361677a4590fab60"
            + "c1c2c3c4"
            + "c1c2c3c4"
            + "40404040"
            + "40404040"
            + "fc5c"
            + "fc5c"
            + "000f"
            + "000f"
            + "0001343a"
            + "0001343a"
            + "000001c4"
            + "000001c4"
            + "0000000000004532456d"
            + "0000000000004532456d"
            + "0000000000007800056f"
            + "0000000000007800056f"
            + "0000000000000000087554907654321c"
            + "0000000000000000087554907654321c"
            + "0000000000000000000564678008321f"
            + "0000000000000000000564678008321f"
            + "000007545f"
            + "000007545f"
            + "45543ae9"
            + "45543ae9"
            + "361677a4590fab60"
            + "361677a4590fab60";
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "alltypes");

        assertEquals("ABCD", dfhcommarea.getSString());
        byte[] cBinary = {0x01, 0x02, 0x00, 0x00};
        assertEquals(HostData.toHexString(cBinary), HostData.toHexString(dfhcommarea.getSBinary()));
        assertEquals((short) -932, dfhcommarea.getSShort());
        assertEquals(15, dfhcommarea.getSUshort());
        assertEquals(78906, dfhcommarea.getSInt());
        assertEquals(452, dfhcommarea.getSUint());
        assertEquals(-4532456, dfhcommarea.getSLong());
        assertEquals(7800056, dfhcommarea.getSUlong());
        assertEquals(new BigInteger("87554907654321"), dfhcommarea.getSXlong());
        assertEquals(new BigInteger("564678008321"), dfhcommarea.getSUxlong());
        assertEquals(new BigDecimal("75.45"), dfhcommarea.getSDec());
        assertEquals(.3450065677999998E+06f, dfhcommarea.getSFloat());
        assertEquals(.7982006699999985E-13d, dfhcommarea.getSDouble());

        for (int i = 0; i < 2; i++) {
            assertEquals("ABCD", dfhcommarea.getAString().get(i));
            assertEquals("", dfhcommarea.getABinary().get(i));
            assertEquals(-932, (int) dfhcommarea.getAShort().get(i));
            assertEquals(15, (int) dfhcommarea.getAUshort().get(i));
            assertEquals(78906, (int) dfhcommarea.getAInt().get(i));
            assertEquals(452, (long) dfhcommarea.getAUint().get(i));
            assertEquals(-4532456, (long) dfhcommarea.getALong().get(i));
            assertEquals(7800056, (long) dfhcommarea.getAUlong().get(i));
            assertEquals(new BigInteger("87554907654321"), dfhcommarea.getAXlong().get(i));
            assertEquals(new BigInteger("564678008321"), dfhcommarea.getAUxlong().get(i));
            assertEquals(new BigDecimal("75.45"), dfhcommarea.getADec().get(i));
            assertEquals(.3450065677999998E+06f, dfhcommarea.getAFloat().get(i));
            assertEquals(.7982006699999985E-13d, dfhcommarea.getADouble().get(i));
        }

    }
}
