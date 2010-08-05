package com.legstar.test.coxb;

import java.util.ArrayList;
import java.util.List;

import com.legstar.test.coxb.arraysdo.Dfhcommarea;
import com.legstar.test.coxb.arraysdo.ObjectFactory;

import junit.framework.TestCase;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public class ArraysdoCases extends TestCase {

    /** Utility class. */
    private ArraysdoCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setTableSize(5);
        List <String> tableOdo = new ArrayList <String>();
        tableOdo.add("ODO01");
        tableOdo.add("ODO02");
        tableOdo.add("ODO03");
        tableOdo.add("ODO04");
        tableOdo.add("ODO05");
        dfhcommarea.getTableOdo().addAll(tableOdo);
        return dfhcommarea;
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() {
        /*       0 5 O D O 0 1 O D O 0 2 O D O 0 3 O D O 0 4 O D O 0 5*/
        return "f0f5d6c4d6f0f1d6c4d6f0f2d6c4d6f0f3d6c4d6f0f4d6c4d6f0f5";
    }
    
    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals(5, dfhcommarea.getTableSize());
        assertEquals("ODO01", dfhcommarea.getTableOdo().get(0));
        assertEquals("ODO02", dfhcommarea.getTableOdo().get(1));
        assertEquals("ODO03", dfhcommarea.getTableOdo().get(2));
        assertEquals("ODO04", dfhcommarea.getTableOdo().get(3));
        assertEquals("ODO05", dfhcommarea.getTableOdo().get(4));
    }

    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }
}
