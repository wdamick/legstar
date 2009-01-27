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

import junit.framework.TestCase;

import com.legstar.test.coxb.redbotha.Filler22;
import com.legstar.test.coxb.redbotha.ObjectFactory;
import com.legstar.test.coxb.redbotha.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class RedbothaCases extends TestCase {

    /** Utility class. */
    private RedbothaCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setCNumeric(5);
        return dfhcommarea;
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "0005";
    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObjectSecondChoice() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        Filler22 filler22 = new Filler22();
        filler22.setCLeftByte("A");
        filler22.setCRightByte("B");
        dfhcommarea.setFiller22(filler22);
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObjectSecondChoice(final Dfhcommarea dfhcommarea) {
        assertEquals(49602, dfhcommarea.getCNumeric().intValue());
        assertEquals("A", dfhcommarea.getFiller22().getCLeftByte());
        assertEquals("B", dfhcommarea.getFiller22().getCRightByte());
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexSecondChoice() { 

        return "c1c2";
    }
}
