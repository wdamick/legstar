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

public class UnmarshalAlltypesTest extends TestCase {

    public void testAlltypes() throws Exception {

        String hexString   = "c1c2c3c401020000fc5c000f0001343a000001c40000000000004532456d0000000000007800056f0000000000000000087554907654321c0000000000000000000564678008321f000007545f45543ae9361677a4590fab60c1c2c3c4c1c2c3c44040404040404040fc5cfc5c000f000f0001343a0001343a000001c4000001c40000000000004532456d0000000000004532456d0000000000007800056f0000000000007800056f0000000000000000087554907654321c0000000000000000087554907654321c0000000000000000000564678008321f0000000000000000000564678008321f000007545f000007545f45543ae945543ae9361677a4590fab60361677a4590fab60";
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "alltypes");

        assertEquals("ABCD", Dfhcommarea.getSString());
        byte[] cBinary = {0x01,0x02,0x00,0x00};
        assertEquals(HostData.toHexString(cBinary), HostData.toHexString(Dfhcommarea.getSBinary()));
        assertEquals((short)-932, Dfhcommarea.getSShort());
        assertEquals(15,Dfhcommarea.getSUshort());
        assertEquals(78906,Dfhcommarea.getSInt());
        assertEquals(452,Dfhcommarea.getSUint());
        assertEquals(-4532456,Dfhcommarea.getSLong());
        assertEquals(7800056,Dfhcommarea.getSUlong());
        assertEquals(new BigInteger("87554907654321"),Dfhcommarea.getSXlong());
        assertEquals(new BigInteger("564678008321"),Dfhcommarea.getSUxlong());
        assertEquals(new BigDecimal("75.45"),Dfhcommarea.getSDec());
        assertEquals(.3450065677999998E+06f,Dfhcommarea.getSFloat());
        assertEquals(.7982006699999985E-13d,Dfhcommarea.getSDouble());

        for (int i = 0; i < 2; i++) {
            assertEquals("ABCD", Dfhcommarea.getAString().get(i));
            assertEquals("", Dfhcommarea.getABinary().get(i));
            assertEquals(-932, (int)Dfhcommarea.getAShort().get(i));
            assertEquals(15,(int)Dfhcommarea.getAUshort().get(i));
            assertEquals(78906,(int)Dfhcommarea.getAInt().get(i));
            assertEquals(452,(long)Dfhcommarea.getAUint().get(i));
            assertEquals(-4532456,(long)Dfhcommarea.getALong().get(i));
            assertEquals(7800056,(long)Dfhcommarea.getAUlong().get(i));
            assertEquals(new BigInteger("87554907654321"),Dfhcommarea.getAXlong().get(i));
            assertEquals(new BigInteger("564678008321"),Dfhcommarea.getAUxlong().get(i));
            assertEquals(new BigDecimal("75.45"),Dfhcommarea.getADec().get(i));
            assertEquals(.3450065677999998E+06f,Dfhcommarea.getAFloat().get(i));
            assertEquals(.7982006699999985E-13d,Dfhcommarea.getADouble().get(i));
        }

    }
}
