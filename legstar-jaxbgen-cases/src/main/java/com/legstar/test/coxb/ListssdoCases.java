package com.legstar.test.coxb;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import com.legstar.test.coxb.listssdo.ObjectFactory;
import com.legstar.test.coxb.listssdo.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class ListssdoCases extends TestCase {

    /** Utility class. */
    private ListssdoCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        List < String > listOdo = new ArrayList < String >();
        listOdo.add("ODO01");
        listOdo.add("ODO02");
        listOdo.add("ODO03");
        listOdo.add("ODO04");
        listOdo.add("ODO05");
        dfhcommarea.getListOdo().addAll(listOdo);
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals(5, dfhcommarea.getListOdo().size());
        assertEquals("ODO01", dfhcommarea.getListOdo().get(0));
        assertEquals("ODO02", dfhcommarea.getListOdo().get(1));
        assertEquals("ODO03", dfhcommarea.getListOdo().get(2));
        assertEquals("ODO04", dfhcommarea.getListOdo().get(3));
        assertEquals("ODO05", dfhcommarea.getListOdo().get(4));
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "00000005"
        + "d6c4d6f0f1"
        + "d6c4d6f0f2"
        + "d6c4d6f0f3"
        + "d6c4d6f0f4"
        + "d6c4d6f0f5";
    }
}
