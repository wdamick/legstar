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

import com.legstar.test.coxb.doublmix.ObjectFactory;
import com.legstar.test.coxb.doublmix.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public class DoublmixCases extends TestCase {

    /** Utility class. */
    private DoublmixCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setCDouble0(0d);
        dfhcommarea.setCDouble1(1d);
        dfhcommarea.setCDouble1234(1234d);
        dfhcommarea.setCDouble345006P5678(345006.5678d);
        dfhcommarea.setCDouble3P40282347Ep38(3.40282347E+38);
        dfhcommarea.setCDouble798P20067Em16(798.20067E-16);

        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals(0d, dfhcommarea.getCDouble0());
        assertEquals(1d, dfhcommarea.getCDouble1());
        assertEquals(1234d, dfhcommarea.getCDouble1234());
        assertEquals(345006.56779999996d, dfhcommarea.getCDouble345006P5678());
        assertEquals(3.40282347E+38, dfhcommarea.getCDouble3P40282347Ep38());
        assertEquals(7.982006699999995E-14, dfhcommarea.getCDouble798P20067Em16());
    }
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "434d2000000000000000000000000000411000000000000045543ae915b573e8361677a4590fab6860ffffff048ff9e8";
    }

    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }
}
