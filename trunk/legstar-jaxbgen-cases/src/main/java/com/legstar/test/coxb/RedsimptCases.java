package com.legstar.test.coxb;

import junit.framework.TestCase;

import com.legstar.test.coxb.redsimpt.ObjectFactory;
import com.legstar.test.coxb.redsimpt.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class RedsimptCases extends TestCase {

    /** Utility class. */
    private RedsimptCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setCDefinition1("ABCDEFGHIJKLMNO");
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals("ABCDEFGHIJKLMNO", dfhcommarea.getCDefinition1());
        
    }
    
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "c1c2c3c4c5c6c7c8c9d1d2d3d4d5d6404040";
    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObjectSecondChoice() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setCDefinition2(123456789012345L);
        return dfhcommarea;
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexSecondChoice() { 

        return "f0f0f0f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5";
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObjectSecondChoice(final Dfhcommarea dfhcommarea) {
        assertEquals(123456789012345L, dfhcommarea.getCDefinition2().longValue());
    }
}
