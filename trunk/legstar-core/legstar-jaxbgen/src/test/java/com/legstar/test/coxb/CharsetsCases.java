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

import com.legstar.test.coxb.charsets.ObjectFactory;
import com.legstar.test.coxb.charsets.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.
 * This is meant to be used with the french character set IMB01147.
 * That character set is not DBCS. 
 */
public class CharsetsCases extends TestCase {

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
     * @return an XML serialization of the object
     */
    public static String getXml() {
        return "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
        + "<Dfhcommarea xmlns=\"http://legstar.com/test/coxb/charsets\">"
        + "<ComLocal>ça c'est un problème</ComLocal>"
        + "<ComDbcs></ComDbcs>"
        + "<ComNational>élémentaire à résoudre          </ComNational>"
        + "</Dfhcommarea>";
    }

    /**
     * @return an XML serialization of the object
     */
    public static String getXmlIBM01140() {
        return "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
        + "<Dfhcommarea xmlns=\"http://legstar.com/test/coxb/charsets\">"
        + "<ComLocal>\\a c'est un probl}me</ComLocal>"
        + "<ComDbcs></ComDbcs>"
        + "<ComNational>élémentaire à résoudre          </ComNational>"
        + "</Dfhcommarea>";
    }
    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals("ça c'est un problème", dfhcommarea.getComLocal());
        assertEquals("", dfhcommarea.getComDbcs());
        assertEquals("élémentaire à résoudre          ", dfhcommarea.getComNational());
    }
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "e08140837d85a2a340a495409799968293d09485404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "00e9006c00e9006d0065006e00740061006900720065002000e00020007200e90073006f0075006400720065"
        + "0020002000200020002000200020002000200020";
    }

    /**
     * @return a hexadecimal representation of host data with a US character set.
     */
    public static String getHostBytesHexIBM01140() { 

        return "488140837d85a2a340a495409799968293549485404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "00e9006c00e9006d0065006e00740061006900720065002000e00020007200e90073006f0075006400720065"
        + "0020002000200020002000200020002000200020";
    }
    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }

}
