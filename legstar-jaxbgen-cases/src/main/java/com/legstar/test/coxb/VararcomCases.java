package com.legstar.test.coxb;

import java.util.Formatter;
import java.util.Locale;

import junit.framework.TestCase;

import com.legstar.test.coxb.vararcom.CArray;
import com.legstar.test.coxb.vararcom.ObjectFactory;
import com.legstar.test.coxb.vararcom.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class VararcomCases extends TestCase {

    /** Utility class. */
    private VararcomCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObjectEmpty() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setCItemsNumber(Short.parseShort("0"));
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObjectEmpty(final Dfhcommarea dfhcommarea) {
        assertEquals(0, dfhcommarea.getCItemsNumber());
    }
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexEmpty() { 

        return "0000";
    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObjectSome() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setCItemsNumber(Short.parseShort("10"));
        for (int i = 0; i < 10; i++) {
            CArray item = new CArray();
            item.setCItem1("ABJAD");
            item.setCItem2(Short.parseShort(Integer.toString(7 * i)));
            dfhcommarea.getCArray().add(item);
        }
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObjectSome(final Dfhcommarea dfhcommarea) {
        assertEquals(10, dfhcommarea.getCItemsNumber());
        for (int i = 0; i < 10; i++) {
            com.legstar.test.coxb.vararcom.CArray item = dfhcommarea.getCArray().get(i);
            assertEquals("ABJAD", item.getCItem1());
            assertEquals(Short.parseShort(Integer.toString(7 * i)), item.getCItem2());
        }
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexSome() { 

        return "000a"
        + "c1c2d1c1c40000"
        + "c1c2d1c1c40007"
        + "c1c2d1c1c4000e"
        + "c1c2d1c1c40015"
        + "c1c2d1c1c4001c"
        + "c1c2d1c1c40023"
        + "c1c2d1c1c4002a"
        + "c1c2d1c1c40031"
        + "c1c2d1c1c40038"
        + "c1c2d1c1c4003f";
    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObjectFull() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setCItemsNumber(Short.parseShort("250"));
        for (int i = 0; i < 250; i++) {
            CArray item = new CArray();
            item.setCItem1("ABJAD");
            item.setCItem2(Short.parseShort(Integer.toString(7 * i)));
            dfhcommarea.getCArray().add(item);
        }
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObjectFull(final Dfhcommarea dfhcommarea) {
        assertEquals(250, dfhcommarea.getCItemsNumber());
        for (int i = 0; i < 250; i++) {
            com.legstar.test.coxb.vararcom.CArray item = dfhcommarea.getCArray().get(i);
            assertEquals("ABJAD", item.getCItem1());
            assertEquals(Short.parseShort(Integer.toString(7 * i)), item.getCItem2());
        }
    }

    /**
     * Check that content is actually returned by vararcom.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObjectVararcom(final Dfhcommarea dfhcommarea) {
        assertEquals(36, dfhcommarea.getCItemsNumber());
        for (int i = 0; i < 36; i++) {
            com.legstar.test.coxb.vararcom.CArray item = dfhcommarea.getCArray().get(i);
            assertEquals("FGHIJ", item.getCItem1());
            assertEquals(Short.parseShort(Integer.toString(5 * (i + 1))), item.getCItem2());
        }
    }
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexFull() {
        
        StringBuilder sb = new StringBuilder("00fa");
        for (int i = 0; i < 250; i++) {
            StringBuilder sbl = new StringBuilder();
            Formatter formatter = new Formatter(sbl, Locale.US);
            formatter.format("%04x", 7 * i);
            sb.append("c1c2d1c1c4" + sbl.toString());
        }

        return sb.toString();
    }
}
