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

public class MarshalAlltypesTest extends TestCase {

	public void testAlltypes() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.getJaxbObject("alltypes");
		
		Dfhcommarea.setSString("ABCD");
        byte[] cBinary = {0x01,0x02};
		Dfhcommarea.setSBinary(cBinary);
		Dfhcommarea.setSShort((short)-932);
		Dfhcommarea.setSUshort(15);
		Dfhcommarea.setSInt(78906);
		Dfhcommarea.setSUint(452);
		Dfhcommarea.setSLong(-4532456);
		Dfhcommarea.setSUlong(7800056);
		Dfhcommarea.setSXlong(new BigInteger("87554907654321"));
		Dfhcommarea.setSUxlong(new BigInteger("564678008321"));
		Dfhcommarea.setSDec(new BigDecimal("75.45"));
		Dfhcommarea.setSFloat(.3450065677999998E+06f);
		Dfhcommarea.setSDouble(.7982006699999985E-13d);
        
        for (int i = 0; i < 2; i++) {
            Dfhcommarea.getAString().add("ABCD");
            Dfhcommarea.getABinary().add("  ");
            Dfhcommarea.getAShort().add((short)-932);
            Dfhcommarea.getAUshort().add(15);
            Dfhcommarea.getAInt().add(78906);
            Dfhcommarea.getAUint().add(452l);
            Dfhcommarea.getALong().add(-4532456l);
            Dfhcommarea.getAUlong().add(7800056l);
            Dfhcommarea.getAXlong().add(new BigInteger("87554907654321"));
            Dfhcommarea.getAUxlong().add(new BigInteger("564678008321"));
            Dfhcommarea.getADec().add(new BigDecimal("75.45"));
            Dfhcommarea.getAFloat().add(.3450065677999998E+06f);
            Dfhcommarea.getADouble().add(.7982006699999985E-13d);
        }

		assertEquals("c1c2c3c401020000fc5c000f0001343a000001c40000000000004532456d0000000000007800056f0000000000000000087554907654321c0000000000000000000564678008321f000007545f45543ae9361677a4590fab60c1c2c3c4c1c2c3c44040404040404040fc5cfc5c000f000f0001343a0001343a000001c4000001c40000000000004532456d0000000000004532456d0000000000007800056f0000000000007800056f0000000000000000087554907654321c0000000000000000087554907654321c0000000000000000000564678008321f0000000000000000000564678008321f000007545f000007545f45543ae945543ae9361677a4590fab60361677a4590fab60",
				Util.marshal("alltypes", Dfhcommarea, 267));
	}
}
