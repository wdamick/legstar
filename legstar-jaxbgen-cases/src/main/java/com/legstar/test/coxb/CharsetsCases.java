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

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.charsets.ObjectFactory;
import com.legstar.test.coxb.charsets.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class CharsetsCases extends TestCase {

    /** Utility class. */
    private CharsetsCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();

        dfhcommarea.setComLocal("ça c'est un problème");
        dfhcommarea.setComNational("élémentaire à résoudre");

        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals("ça c'est un problème", dfhcommarea.getComLocal());
        assertEquals("00000000000000000000000000000000000000000000000000"
        + "0000000000000000000000000000000000000000000000000000000000000"
        + "00000000000000000", HostData.toHexString(dfhcommarea.getComDbcs()));
        assertEquals("élémentaire à résoudre          ", dfhcommarea.getComNational());
    }
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "e08140837d85a2a340a495409799968293d09485404040404040404040404040"
        + "000000000000000000000000000000000000000000000000000000000000000000000000000"
        + "0000000000000000000000000000000000000000000000000000000"
        + "e9006c00e9006d0065006e00740061006900720065002000e00020007200e90073006f0075006400720065"
        + "0020002000200020002000200020002000200020";
    }

    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }

}
