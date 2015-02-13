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

import junit.framework.TestCase;

import com.legstar.test.coxb.redopera.Filler25;
import com.legstar.test.coxb.redopera.Filler28;
import com.legstar.test.coxb.redopera.ObjectFactory;
import com.legstar.test.coxb.redopera.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public class RedoperaCases extends TestCase {

    /** Utility class. */
    private RedoperaCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setCFunction("stringMethod");
        Filler25 filler25 = of.createFiller25();
        filler25.setCString("ABJADHAOUAZ");
        dfhcommarea.setFiller25(filler25);
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals("ABJADHAOUAZ", dfhcommarea.getFiller25().getCString());
    }
    
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "a2a399899587d485a3889684404040404040"
        + "c1c2d1c1c4c8c1d6e4c1e9404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040";
    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObjectIntMethod() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setCFunction("intMethod");
        Filler28 filler28 = of.createFiller28();
        filler28.setCInteger(345);
        dfhcommarea.setFiller28(filler28);
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObjectIntMethod(final Dfhcommarea dfhcommarea) {
        assertEquals(null, dfhcommarea.getFiller25());
        assertEquals(345, dfhcommarea.getFiller28().getCInteger());
    }
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexIntMethod() { 

        return "8995a3d485a3889684404040404040404040"
        + "f0f0f0f0f0f3f4c5"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "404040404040404040404040";
    }

    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }
}
