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

import com.legstar.test.coxb.redmulti.Filler35;
import com.legstar.test.coxb.redmulti.Filler38;
import com.legstar.test.coxb.redmulti.ObjectFactory;
import com.legstar.test.coxb.redmulti.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public class RedmultiCases extends TestCase {

    /** Utility class. */
    private RedmultiCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setCOutputType("normal");
        Filler35 filler35 = new Filler35();
        filler35.setCString("ABJADHAOUAZ");
        dfhcommarea.setFiller35(filler35);
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals("normal", dfhcommarea.getCOutputType());
        assertEquals("ABJADHAOUAZ", dfhcommarea.getFiller35().getCString());
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "959699948193"
        + "c1c2d1c1c4c8c1d6e4c1"
        + "e9404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040";
    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObjectError() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setCOutputType("error");
        Filler38 filler38 = new Filler38();
        filler38.setCErrorNum(75);
        filler38.setCErrorDescription("ABOMINABLE");
        dfhcommarea.setFiller38(filler38);
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObjectError(final Dfhcommarea dfhcommarea) {
        assertEquals("error", dfhcommarea.getCOutputType());
        assertEquals(75, dfhcommarea.getFiller38().getCErrorNum());
        assertEquals("ABOMINABLE", dfhcommarea.getFiller38().getCErrorDescription());
    }
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexError() { 

        return "859999969940"
        + "f0f0f7f5"
        + "c1c2d6d4c9d5"
        + "c1c2d3c5404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040";
    }

    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }
}
