package com.legstar.test.coxb;



import java.math.BigDecimal;
import java.math.BigInteger;

import com.legstar.coxb.host.HostData;

import junit.framework.TestCase;
import com.legstar.test.coxb.alltypes.DfhcommareaType;

public class UnmarshalAlltypesTest extends TestCase {

	public void testAlltypes() throws Exception {

		String hexString   = "c1c2c3c401020000fc5c000f0001343a000001c40000000000004532456d0000000000007800056f0000000000000000087554907654321c0000000000000000000564678008321f000007545f45543ae9361677a4590fab60c1c2c3c4c1c2c3c44040404040404040fc5cfc5c000f000f0001343a0001343a000001c4000001c40000000000004532456d0000000000004532456d0000000000007800056f0000000000007800056f0000000000000000087554907654321c0000000000000000087554907654321c0000000000000000000564678008321f0000000000000000000564678008321f000007545f000007545f45543ae945543ae9361677a4590fab60361677a4590fab60";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "alltypes");
		
        assertEquals("ABCD", dfhcommareaType.getSString());
        byte[] cBinary = {0x01,0x02,0x00,0x00};
        assertEquals(HostData.toHexString(cBinary), HostData.toHexString(dfhcommareaType.getSBinary()));
        assertEquals((short)-932, dfhcommareaType.getSShort());
        assertEquals(15,dfhcommareaType.getSUshort());
        assertEquals(78906,dfhcommareaType.getSInt());
        assertEquals(452,dfhcommareaType.getSUint());
        assertEquals(-4532456,dfhcommareaType.getSLong());
        assertEquals(7800056,dfhcommareaType.getSUlong());
        assertEquals(new BigInteger("87554907654321"),dfhcommareaType.getSXlong());
        assertEquals(new BigInteger("564678008321"),dfhcommareaType.getSUxlong());
        assertEquals(new BigDecimal("75.45"),dfhcommareaType.getSDec());
        assertEquals(.3450065677999998E+06f,dfhcommareaType.getSFloat());
        assertEquals(.7982006699999985E-13d,dfhcommareaType.getSDouble());
        
        for (int i = 0; i < 2; i++) {
            assertEquals("ABCD", dfhcommareaType.getAString().get(i));
            assertEquals("", dfhcommareaType.getABinary().get(i));
            assertEquals(-932, (int)dfhcommareaType.getAShort().get(i));
            assertEquals(15,(int)dfhcommareaType.getAUshort().get(i));
            assertEquals(78906,(int)dfhcommareaType.getAInt().get(i));
            assertEquals(452,(long)dfhcommareaType.getAUint().get(i));
            assertEquals(-4532456,(long)dfhcommareaType.getALong().get(i));
            assertEquals(7800056,(long)dfhcommareaType.getAUlong().get(i));
            assertEquals(new BigInteger("87554907654321"),dfhcommareaType.getAXlong().get(i));
            assertEquals(new BigInteger("564678008321"),dfhcommareaType.getAUxlong().get(i));
            assertEquals(new BigDecimal("75.45"),dfhcommareaType.getADec().get(i));
            assertEquals(.3450065677999998E+06f,dfhcommareaType.getAFloat().get(i));
            assertEquals(.7982006699999985E-13d,dfhcommareaType.getADouble().get(i));
        }

	}
}
