package com.legstar.test.coxb;

import junit.framework.TestCase;

import com.legstar.test.coxb.floatmix.ObjectFactory;
import com.legstar.test.coxb.floatmix.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class FloatmixCases extends TestCase {

    /** Utility class. */
    private FloatmixCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setCFloat0(0f);
        dfhcommarea.setCFloat1(1f);
        dfhcommarea.setCFloat1234(1234f);
        dfhcommarea.setCFloat345006P5678(345006.5678f);
        dfhcommarea.setCFloat3P40282347Ep38(3.40282347E+38f);
        dfhcommarea.setCFloat798P20067Em16(798.20067E-16f);
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals(0f, dfhcommarea.getCFloat0());
        assertEquals(1f, dfhcommarea.getCFloat1());
        assertEquals(1234f, dfhcommarea.getCFloat1234());
        assertEquals(345006.56779999996f, dfhcommarea.getCFloat345006P5678());
        assertEquals(3.40282347E+38f, dfhcommarea.getCFloat3P40282347Ep38());
        assertEquals(7.982005E-14f, dfhcommarea.getCFloat798P20067Em16());
    }
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "434d2000000000004110000045543ae9361677a460ffffff";
    }
}
