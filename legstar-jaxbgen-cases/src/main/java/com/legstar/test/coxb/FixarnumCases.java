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
import java.math.BigInteger;

import junit.framework.TestCase;

import com.legstar.test.coxb.fixarnum.ObjectFactory;
import com.legstar.test.coxb.fixarnum.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public class FixarnumCases extends TestCase {

    /** Utility class. */
    private FixarnumCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.getCArrayPd().add(new BigDecimal("16534.23"));
        dfhcommarea.getCArrayPd().add(new BigDecimal("1.5"));
        dfhcommarea.getCArrayPd().add(new BigDecimal("184"));

        dfhcommarea.getCArrayZd().add(new BigDecimal("534.236"));
        dfhcommarea.getCArrayZd().add(new BigDecimal("45.007"));
        dfhcommarea.getCArrayZd().add(new BigDecimal("1.95"));

        dfhcommarea.getCArrayZi().add(new Integer("9998"));
        dfhcommarea.getCArrayZi().add(new Integer("0"));
        dfhcommarea.getCArrayZi().add(new Integer("178"));

        dfhcommarea.getCArrayBi().add(new Long("999899998"));
        dfhcommarea.getCArrayBi().add(new Long("676767"));
        dfhcommarea.getCArrayBi().add(new Long("36789013"));

        dfhcommarea.getCArrayNi().add(new BigInteger("123456789012345678"));
        dfhcommarea.getCArrayNi().add(new BigInteger("6767679998"));
        dfhcommarea.getCArrayNi().add(new BigInteger("36789184"));
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals("16534.23", dfhcommarea.getCArrayPd().get(0).toString());
        assertEquals("1.50", dfhcommarea.getCArrayPd().get(1).toString());
        assertEquals("184.00", dfhcommarea.getCArrayPd().get(2).toString());

        assertEquals("534.236", dfhcommarea.getCArrayZd().get(0).toString());
        assertEquals("45.007", dfhcommarea.getCArrayZd().get(1).toString());
        assertEquals("1.950", dfhcommarea.getCArrayZd().get(2).toString());

        assertEquals("9998", dfhcommarea.getCArrayZi().get(0).toString());
        assertEquals("0", dfhcommarea.getCArrayZi().get(1).toString());
        assertEquals("178", dfhcommarea.getCArrayZi().get(2).toString());

        assertEquals("999899998", dfhcommarea.getCArrayBi().get(0).toString());
        assertEquals("676767", dfhcommarea.getCArrayBi().get(1).toString());
        assertEquals("36789013", dfhcommarea.getCArrayBi().get(2).toString());

        assertEquals("123456789012345678", dfhcommarea.getCArrayNi().get(0).toString());
        assertEquals("6767679998", dfhcommarea.getCArrayNi().get(1).toString());
        assertEquals("36789184", dfhcommarea.getCArrayNi().get(2).toString());
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "1653423f"
        + "0000150f"
        + "0018400f"
        + "f5f3f4f2f3f6"
        + "f0f4f5f0f0f7"
        + "f0f0f1f9f5f0"
        + "f9f9f9f8"
        + "f0f0f0f0"
        + "f0f1f7f8"
        + "3b99435e000a539f02315b1501b69b4ba630f34e00000001936299fe0000000002315bc0";
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexEmpty() { 

        return "0000000f"
        + "0000000f"
        + "0000000f"
        + "f0f0f0f0f0f0"
        + "f0f0f0f0f0f0"
        + "f0f0f0f0f0f0"
        + "f0f0f0f0"
        + "f0f0f0f0"
        + "f0f0f0f0"
        + "000000000000000000000000000000000000000000000000000000000000000000000000";
    }

    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }

}
