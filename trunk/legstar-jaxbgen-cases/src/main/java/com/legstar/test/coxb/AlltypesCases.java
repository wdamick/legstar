/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.test.coxb;

import java.math.BigDecimal;
import java.math.BigInteger;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.alltypes.Dfhcommarea;
import com.legstar.test.coxb.alltypes.ObjectFactory;

import junit.framework.TestCase;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class AlltypesCases extends TestCase {

    /** Utility class. */
    private AlltypesCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();

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

        return dfhcommarea;
    }
    
    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
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

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "c1c2c3c4"
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
    }

    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }

}
