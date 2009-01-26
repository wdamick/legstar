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

import com.legstar.test.coxb.redinout.CParain;
import com.legstar.test.coxb.redinout.CParaout;
import com.legstar.test.coxb.redinout.ObjectFactory;
import com.legstar.test.coxb.redinout.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class RedinoutCases extends TestCase {

    /** Utility class. */
    private RedinoutCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setCNumeric(35);
        CParain parain = new CParain();
        parain.setCSomeInput("ABCDEABCDEABCDE");
        dfhcommarea.setCParain(parain);
        return dfhcommarea;
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "0023c1c2c3c4c5c1c2c3c4c5c1c2c3c4c5";
    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObjectSecondChoice() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setCNumeric(35);
        CParaout paraout = new CParaout();
        paraout.setCSomeOutput(12345678);
        dfhcommarea.setCParaout(paraout);
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObjectSecondChoice(final Dfhcommarea dfhcommarea) {
        assertEquals(35, dfhcommarea.getCNumeric());
        assertEquals(12345678, dfhcommarea.getCParaout().getCSomeOutput());
        assertEquals(null, dfhcommarea.getCParain());
    }
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexSecondChoice() { 

        return "0023f1f2f3f4f5f6f7f84040404040404040404040404040404040";
    }
}
