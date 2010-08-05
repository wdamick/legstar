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

import com.legstar.test.coxb.numzoned.ObjectFactory;
import com.legstar.test.coxb.numzoned.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public class NumzonedCases extends TestCase {

    /** Utility class. */
    private NumzonedCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setLU(6);
        dfhcommarea.setLS(Short.parseShort("-5"));
        dfhcommarea.setLSSignL(Short.parseShort("-78"));
        dfhcommarea.setLSSignT(Short.parseShort("1"));
        dfhcommarea.setLSSignSL(Short.parseShort("9"));
        dfhcommarea.setLSSignST(Short.parseShort("-11"));
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals(6, dfhcommarea.getLU());
        assertEquals(-5, dfhcommarea.getLS());
        assertEquals(-78, dfhcommarea.getLSSignL());
        assertEquals(1, dfhcommarea.getLSSignT());
        assertEquals(9, dfhcommarea.getLSSignSL());
        assertEquals(-11, dfhcommarea.getLSSignST());
    }
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "f6f0d5d0f7f8f0c14ef9f1f160";
    }

    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }
}
