package com.legstar.test.coxb;

import junit.framework.TestCase;

import com.legstar.test.coxb.lsfileae.ObjectFactory;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;
import com.legstar.test.coxb.lsfileae.ComPersonal;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class LsfileaeCases extends TestCase {

    /** Utility class. */
    private LsfileaeCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setComNumber(100L);
        ComPersonal comPersonal = new ComPersonal();
        comPersonal.setComName("TOTO");
        comPersonal.setComAddress("LABAS STREET");
        comPersonal.setComPhone("88993314");
        dfhcommarea.setComPersonal(comPersonal);
        dfhcommarea.setComDate("100458");
        dfhcommarea.setComAmount("00100.35");
        dfhcommarea.setComComment("A VOIR");
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals(100, dfhcommarea.getComNumber());
        assertEquals("TOTO", dfhcommarea.getComPersonal().getComName().trim());
        assertEquals("LABAS STREET", dfhcommarea.getComPersonal().getComAddress().trim());
        assertEquals("88993314", dfhcommarea.getComPersonal().getComPhone().trim());
        assertEquals("100458", dfhcommarea.getComDate().trim());
        assertEquals("00100.35", dfhcommarea.getComAmount().trim());
        assertEquals("A VOIR", dfhcommarea.getComComment().trim());
    }
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "f0f0f0f1f0f0"
        + "e3d6e3d640404040404040404040404040404040"
        + "d3c1c2c1e240e2e3d9c5c5e34040404040404040"
        + "f8f8f9f9f3f3f1f4"
        + "f1f0f0f4f5f84040"
        + "f0f0f1f0f04bf3f5"
        + "c140e5d6c9d9404040";
    }
}
