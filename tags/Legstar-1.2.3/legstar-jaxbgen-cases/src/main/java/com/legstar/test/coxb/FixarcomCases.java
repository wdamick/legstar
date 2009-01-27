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

import com.legstar.test.coxb.fixarcom.CArray;
import com.legstar.test.coxb.fixarcom.ObjectFactory;
import com.legstar.test.coxb.fixarcom.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class FixarcomCases extends TestCase {

    /** Utility class. */
    private FixarcomCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        for (int i = 0; i < 7; i++) {
            CArray cArray = new CArray();
            cArray.setCItem1("ABJA" + Integer.toString(i));
            cArray.setCItem2(Short.parseShort(Integer.toString(7 * i)));
            dfhcommarea.getCArray().add(cArray);
        }
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        for (int i = 0; i < 7; i++) {
            com.legstar.test.coxb.fixarcom.CArray item = dfhcommarea.getCArray().get(i);
            assertEquals("ABJA" + Integer.toString(i), item.getCItem1());
            assertEquals(Short.parseShort(Integer.toString(7 * i)), item.getCItem2());
        }
    }
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "c1c2d1c1f00000"
        + "c1c2d1c1f10007"
        + "c1c2d1c1f2000e"
        + "c1c2d1c1f30015"
        + "c1c2d1c1f4001c"
        + "c1c2d1c1f50023"
        + "c1c2d1c1f6002a";
    }
}
