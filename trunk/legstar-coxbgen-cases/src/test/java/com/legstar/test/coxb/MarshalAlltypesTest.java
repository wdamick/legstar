package com.legstar.test.coxb;



import java.math.BigDecimal;
import java.math.BigInteger;

import com.legstar.test.coxb.alltypes.DfhcommareaType;

import junit.framework.TestCase;

public class MarshalAlltypesTest extends TestCase {

	public void testAlltypes() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject("alltypes");
		
		dfhcommareaType.setSString("ABCD");
        byte[] cBinary = {0x01,0x02};
		dfhcommareaType.setSBinary(cBinary);
		dfhcommareaType.setSShort((short)-932);
		dfhcommareaType.setSUshort(15);
		dfhcommareaType.setSInt(78906);
		dfhcommareaType.setSUint(452);
		dfhcommareaType.setSLong(-4532456);
		dfhcommareaType.setSUlong(7800056);
		dfhcommareaType.setSXlong(new BigInteger("87554907654321"));
		dfhcommareaType.setSUxlong(new BigInteger("564678008321"));
		dfhcommareaType.setSDec(new BigDecimal("75.45"));
		dfhcommareaType.setSFloat(.3450065677999998E+06f);
		dfhcommareaType.setSDouble(.7982006699999985E-13d);
        
        for (int i = 0; i < 2; i++) {
            dfhcommareaType.getAString().add("ABCD");
            dfhcommareaType.getABinary().add("  ");
            dfhcommareaType.getAShort().add((short)-932);
            dfhcommareaType.getAUshort().add(15);
            dfhcommareaType.getAInt().add(78906);
            dfhcommareaType.getAUint().add(452l);
            dfhcommareaType.getALong().add(-4532456l);
            dfhcommareaType.getAUlong().add(7800056l);
            dfhcommareaType.getAXlong().add(new BigInteger("87554907654321"));
            dfhcommareaType.getAUxlong().add(new BigInteger("564678008321"));
            dfhcommareaType.getADec().add(new BigDecimal("75.45"));
            dfhcommareaType.getAFloat().add(.3450065677999998E+06f);
            dfhcommareaType.getADouble().add(.7982006699999985E-13d);
        }

		assertEquals("c1c2c3c401020000fc5c000f0001343a000001c40000000000004532456d0000000000007800056f0000000000000000087554907654321c0000000000000000000564678008321f000007545f45543ae9361677a4590fab60c1c2c3c4c1c2c3c44040404040404040fc5cfc5c000f000f0001343a0001343a000001c4000001c40000000000004532456d0000000000004532456d0000000000007800056f0000000000007800056f0000000000000000087554907654321c0000000000000000087554907654321c0000000000000000000564678008321f0000000000000000000564678008321f000007545f000007545f45543ae945543ae9361677a4590fab60361677a4590fab60",
				Util.marshal("alltypes", dfhcommareaType, 267));
	}
}
