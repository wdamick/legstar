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

import com.legstar.test.coxb.fixarsim.ObjectFactory;
import com.legstar.test.coxb.fixarsim.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public class FixarsimCases extends TestCase {

    /** Utility class. */
    private FixarsimCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.getCArray().add("PREMI");
        dfhcommarea.getCArray().add("DEUXI");
        dfhcommarea.getCArray().add("TROIS");
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals("PREMI", dfhcommarea.getCArray().get(0).trim());
        assertEquals("DEUXI", dfhcommarea.getCArray().get(1).trim());
        assertEquals("TROIS", dfhcommarea.getCArray().get(2).trim());
    }
   /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "d7d9c5d4c9c4c5e4e7c9e3d9d6c9e2";
    }

    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }

}
