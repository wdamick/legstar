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

import junit.framework.TestCase;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.typesmix.ObjectFactory;
import com.legstar.test.coxb.typesmix.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class TypesmixCases extends TestCase {

    /** Utility class. */
    private TypesmixCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setCAlphabetic("ABCDE");
        dfhcommarea.setCNational("ABCDE");
        dfhcommarea.setCDbcs("");
        dfhcommarea.setCAlphanumericEdited("");
        dfhcommarea.setCAlphanumeric("");
        byte[] cOctetString = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
        dfhcommarea.setCOctetString(cOctetString);
        dfhcommarea.setCPackedDecimal(new BigDecimal(0));
        dfhcommarea.setCNumericEdited1("0");
        dfhcommarea.setCNumericEdited2("0");
        dfhcommarea.setCNumericEdited3("0");
        dfhcommarea.setCNumericEdited4("0");
        dfhcommarea.setCExternalFloating("+00.00E+00");
        byte[] cIndex = {0x00, 0x00, 0x00, 0x00};
        dfhcommarea.setCIndex(cIndex);
        byte[] cPointer = {0x00, 0x00, 0x00, 0x00};
        dfhcommarea.setCPointer(cPointer);
        byte[] cProcPointer = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
        dfhcommarea.setCProcPointer(cProcPointer);
        byte[] cFuncPointer = {0x00, 0x00, 0x00, 0x00};
        dfhcommarea.setCFuncPointer(cFuncPointer);
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals("ABCDE", dfhcommarea.getCAlphabetic());
        assertEquals("ABCDE    ", dfhcommarea.getCNational());
        assertEquals("", dfhcommarea.getCDbcs());
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
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "c1c2c3c4c5"
        + "004100420043004400450020002000200020"
        + "4040404040404040"
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
    }
    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }
}
