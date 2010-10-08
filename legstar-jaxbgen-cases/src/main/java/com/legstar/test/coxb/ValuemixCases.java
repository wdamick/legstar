/*******************************************************************************
 * Copyright (c) 2010 LegSem.
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

import com.legstar.test.coxb.valuemix.ObjectFactory;
import com.legstar.test.coxb.valuemix.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public class ValuemixCases extends TestCase {

    /** Utility class. */
    private ValuemixCases() {

    }

    /**
     * @return an instance of a valued java object (uses default values).
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        return of.createDfhcommarea();
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals(0L, dfhcommarea.getWsZero());
        assertEquals(0L, dfhcommarea.getWsZeros());
        assertEquals(0L, dfhcommarea.getWsZeroes());
        assertEquals("", dfhcommarea.getWsSpace());
        assertEquals("", dfhcommarea.getWsSpaces());
        assertEquals("0xFFFFFFFFFF", dfhcommarea.getWsHighValue());
        assertEquals("0xFFFFFFFFFF", dfhcommarea.getWsHighValues());
        assertEquals("0x0000000000", dfhcommarea.getWsLowValue());
        assertEquals("0x0000000000", dfhcommarea.getWsLowValues());
        assertEquals("\'", dfhcommarea.getWsQuote());
        assertEquals("\'", dfhcommarea.getWsQuotes());
        assertEquals("0x0000000000", dfhcommarea.getWsNull());
        assertEquals("0x0000000000", dfhcommarea.getWsNulls());
        assertEquals("ABCDE", dfhcommarea.getWsString());
        assertEquals(-345, dfhcommarea.getWsNumeric());
        assertEquals(new BigDecimal("-245.56"), dfhcommarea.getWsPackedDecimal());
        assertEquals(6.0E7F, dfhcommarea.getWsSingleFloat());
        assertEquals(-1.8E-56D, dfhcommarea.getWsDoubleFloat());
    }
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "f0f0f0f0f0"
        + "f0f0f0f0f0"
        + "f0f0f0f0f0"
        + "4040404040"
        + "4040404040"
        + "ffffffffff"
        + "ffffffffff"
        + "0000000000"
        + "0000000000"
        + "7d40404040"
        + "7d40404040"
        + "0000000000"
        + "0000000000"
        + "c1c2c3c4c5"
        + "f0f0f3f4d5"
        + "00000000000024556d"
        + "47393870"
        + "9270fce28208b620";
    }

    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }
}
