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

import java.util.ArrayList;
import java.util.List;

import com.legstar.test.coxb.arrayssm.Dfhcommarea;
import com.legstar.test.coxb.arrayssm.ObjectFactory;
import com.legstar.test.coxb.arrayssm.TableComplex;
import com.legstar.test.coxb.arrayssm.TableComplex2;

import junit.framework.TestCase;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public class ArrayssmCases extends TestCase {

    /** Utility class. */
    private ArrayssmCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        List <String> tableSimple = new ArrayList <String>();
        tableSimple.add("TS1");
        tableSimple.add("TS2");
        dfhcommarea.getTableSimple().addAll(tableSimple);

        TableComplex tc1 = of.createTableComplex();
        tc1.setElementComplex("TCEC1");
        dfhcommarea.getTableComplex().add(tc1);
        TableComplex tc2 = of.createTableComplex();
        tc2.setElementComplex("TCEC2");
        dfhcommarea.getTableComplex().add(tc2);
        TableComplex tc3 = of.createTableComplex();
        tc3.setElementComplex("TCEC3");
        dfhcommarea.getTableComplex().add(tc3);

        TableComplex2 tcc = of.createTableComplex2();
        tcc.getElementComplex2().add("TC2EC21");
        tcc.getElementComplex2().add("TC2EC22");
        tcc.getElementComplex2().add("TC2EC23");
        tcc.getElementComplex2().add("TC2EC24");
        dfhcommarea.setTableComplex2(tcc);

        List <Integer> tableSimpleNumeric = new ArrayList <Integer>();
        tableSimpleNumeric.add(1);
        tableSimpleNumeric.add(2);
        tableSimpleNumeric.add(3);
        tableSimpleNumeric.add(4);
        tableSimpleNumeric.add(5);
        dfhcommarea.getTableSimpleNumeric().addAll(tableSimpleNumeric);
        return dfhcommarea;
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() {
        /*       T S 1 T S 2*/
        return "e3e2f1e3e2f2"
        /*  T C E C 1 T C E C 2 T C E C 3 */
        + "e3c3c5c3f1e3c3c5c3f2e3c3c5c3f3"
        /*  T C 2 E C 2 1 T C 2 E C 2 2 T C 2 E C 2 3 T C 2 E C 2 4*/
        + "e3c3f2c5c3f2f1e3c3f2c5c3f2f2e3c3f2c5c3f2f3e3c3f2c5c3f2f4"
        /*  1 2 3 4 5*/
        + "f1f2f3f4f5";
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals("TS1", dfhcommarea.getTableSimple().get(0));
        assertEquals("TS2", dfhcommarea.getTableSimple().get(1));
        assertEquals("TCEC1", dfhcommarea.getTableComplex().get(0).getElementComplex());
        assertEquals("TCEC2", dfhcommarea.getTableComplex().get(1).getElementComplex());
        assertEquals("TCEC3", dfhcommarea.getTableComplex().get(2).getElementComplex());
        assertEquals("TC2EC21", dfhcommarea.getTableComplex2().getElementComplex2().get(0));
        assertEquals("TC2EC22", dfhcommarea.getTableComplex2().getElementComplex2().get(1));
        assertEquals("TC2EC23", dfhcommarea.getTableComplex2().getElementComplex2().get(2));
        assertEquals("TC2EC24", dfhcommarea.getTableComplex2().getElementComplex2().get(3));
        assertEquals("1", dfhcommarea.getTableSimpleNumeric().get(0).toString());
        assertEquals("2", dfhcommarea.getTableSimpleNumeric().get(1).toString());
        assertEquals("3", dfhcommarea.getTableSimpleNumeric().get(2).toString());
        assertEquals("4", dfhcommarea.getTableSimpleNumeric().get(3).toString());
        assertEquals("5", dfhcommarea.getTableSimpleNumeric().get(4).toString());
    }
    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }
}
