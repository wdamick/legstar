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

import com.legstar.test.coxb.alltypes.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal alltypes.
 *
 */
public class MarshalAlltypesTest extends TestCase {

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testAlltypes() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.getJaxbObject("alltypes");

        dfhcommarea.setSString("ABCD");
        byte[] cBinary = {0x01, 0x02};
        dfhcommarea.setSBinary(cBinary);
        dfhcommarea.setSShort((short) -932);
        dfhcommarea.setSUshort(15);
        dfhcommarea.setSInt(78906);
        dfhcommarea.setSUint(452);
        dfhcommarea.setSLong(-4532456);
        dfhcommarea.setSUlong(7800056);
        dfhcommarea.setSXlong(new BigInteger("87554907654321"));
        dfhcommarea.setSUxlong(new BigInteger("564678008321"));
        dfhcommarea.setSDec(new BigDecimal("75.45"));
        dfhcommarea.setSFloat(.3450065677999998E+06f);
        dfhcommarea.setSDouble(.7982006699999985E-13d);

        for (int i = 0; i < 2; i++) {
            dfhcommarea.getAString().add("ABCD");
            dfhcommarea.getABinary().add("  ");
            dfhcommarea.getAShort().add((short) -932);
            dfhcommarea.getAUshort().add(15);
            dfhcommarea.getAInt().add(78906);
            dfhcommarea.getAUint().add(452L);
            dfhcommarea.getALong().add(-4532456L);
            dfhcommarea.getAUlong().add(7800056L);
            dfhcommarea.getAXlong().add(new BigInteger("87554907654321"));
            dfhcommarea.getAUxlong().add(new BigInteger("564678008321"));
            dfhcommarea.getADec().add(new BigDecimal("75.45"));
            dfhcommarea.getAFloat().add(.3450065677999998E+06f);
            dfhcommarea.getADouble().add(.7982006699999985E-13d);
        }

        assertEquals("c1c2c3c4"
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
        + "361677a4590fab60",
                Util.marshal("alltypes", dfhcommarea, 267));
    }
}
