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

import java.math.BigInteger;

import com.legstar.test.coxb.binarcht.Dfhcommarea;
import com.legstar.test.coxb.binarcht.LsSignedNative;
import com.legstar.test.coxb.binarcht.LsUnsignedNative;
import com.legstar.test.coxb.binarcht.ObjectFactory;

import junit.framework.TestCase;


/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class BinarchtCases extends TestCase {

    /** Utility class. */
    private BinarchtCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();

        LsSignedNative lsSignedNative = new LsSignedNative();
        lsSignedNative.setLsPs9X18Max(12345678901234567L);
        lsSignedNative.setLsPs9X18Min(-12345678901234567L);
        lsSignedNative.setLsPs9X4Max(new Short("32767"));
        lsSignedNative.setLsPs9X4Min(new Short("-32768"));
        lsSignedNative.setLsPs9X9Max(2147483647);
        lsSignedNative.setLsPs9X9Min(-123456789);

        dfhcommarea.setLsSignedNative(lsSignedNative);

        LsUnsignedNative lsUnsignedNative = new LsUnsignedNative();
        lsUnsignedNative.setLsP9X18Max(new BigInteger("18446744073709551615"));
        lsUnsignedNative.setLsP9X18Min(new BigInteger("0"));
        lsUnsignedNative.setLsP9X4Max(65535);
        lsUnsignedNative.setLsP9X4Min(0);
        lsUnsignedNative.setLsP9X9Max(4294967295L);
        lsUnsignedNative.setLsP9X9Min(0);

        dfhcommarea.setLsUnsignedNative(lsUnsignedNative);

        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals(12345678901234567L,
                dfhcommarea.getLsSignedNative().getLsPs9X18Max());
        assertEquals(-12345678901234567L,
                dfhcommarea.getLsSignedNative().getLsPs9X18Min());
        assertEquals(32767,
                dfhcommarea.getLsSignedNative().getLsPs9X4Max());
        assertEquals(-32768,
                dfhcommarea.getLsSignedNative().getLsPs9X4Min());
        assertEquals(2147483647,
                dfhcommarea.getLsSignedNative().getLsPs9X9Max());
        assertEquals(-123456789,
                dfhcommarea.getLsSignedNative().getLsPs9X9Min());

        assertEquals(65535,
                dfhcommarea.getLsUnsignedNative().getLsP9X4Max());
        assertEquals(0,
                dfhcommarea.getLsUnsignedNative().getLsP9X4Min());
        assertEquals(4294967295L,
                dfhcommarea.getLsUnsignedNative().getLsP9X9Max());
        assertEquals(0L,
                dfhcommarea.getLsUnsignedNative().getLsP9X9Min());
        assertEquals(new BigInteger("18446744073709551615"),
                dfhcommarea.getLsUnsignedNative().getLsP9X18Max());
        assertEquals(new BigInteger("0"),
                dfhcommarea.getLsUnsignedNative().getLsP9X18Min());
    }
    
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "0000"
        + "ffff"
        + "00000000"
        + "ffffffff"
        + "0000000000000000"
        + "ffffffffffffffff"
        + "8000"
        + "7fff"
        + "f8a432eb"
        + "7fffffff"
        + "ffd423aba294b479"
        + "002bdc545d6b4b87";
    }

    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }

}
