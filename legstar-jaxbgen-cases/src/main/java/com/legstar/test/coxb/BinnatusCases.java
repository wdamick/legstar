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

import java.math.BigInteger;

import junit.framework.TestCase;

import com.legstar.test.coxb.binnatus.LsDoublewords;
import com.legstar.test.coxb.binnatus.LsFullwords;
import com.legstar.test.coxb.binnatus.LsHalfwords;
import com.legstar.test.coxb.binnatus.LsUnsignedNative;
import com.legstar.test.coxb.binnatus.ObjectFactory;
import com.legstar.test.coxb.binnatus.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public class BinnatusCases extends TestCase {

    /** Utility class. */
    private BinnatusCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();

        LsUnsignedNative lsUnsignedNative = new LsUnsignedNative(); 

        LsHalfwords lsHalfwords = new LsHalfwords();
        lsHalfwords.setLsP9X4High(32769);
        lsHalfwords.setLsP9X4Low(127);
        lsHalfwords.setLsP9X4Max(65535);
        lsHalfwords.setLsP9X4Min(0);

        LsFullwords lsFullwords = new LsFullwords();
        lsFullwords.setLsP9X9High(2147483649L);
        lsFullwords.setLsP9X9Low(65534);
        lsFullwords.setLsP9X9Max(4294967295L);
        lsFullwords.setLsP9X9Min(0);

        LsDoublewords lsDoublewords = new LsDoublewords();
        lsDoublewords.setLsP9X18High(new BigInteger("18446744069414584318"));
        lsDoublewords.setLsP9X18Low(new BigInteger("4294967294"));
        lsDoublewords.setLsP9X18Max(new BigInteger("18446744073709551615"));
        lsDoublewords.setLsP9X18Min(new BigInteger("0"));

        lsUnsignedNative.setLsHalfwords(lsHalfwords);
        lsUnsignedNative.setLsFullwords(lsFullwords);
        lsUnsignedNative.setLsDoublewords(lsDoublewords);

        dfhcommarea.setLsUnsignedNative(lsUnsignedNative);

        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals(32769,
                dfhcommarea.getLsUnsignedNative().getLsHalfwords().getLsP9X4High());
        assertEquals(127,
                dfhcommarea.getLsUnsignedNative().getLsHalfwords().getLsP9X4Low());
        assertEquals(65535,
                dfhcommarea.getLsUnsignedNative().getLsHalfwords().getLsP9X4Max());
        assertEquals(0,
                dfhcommarea.getLsUnsignedNative().getLsHalfwords().getLsP9X4Min());

        assertEquals(2147483649L,
                dfhcommarea.getLsUnsignedNative().getLsFullwords().getLsP9X9High());
        assertEquals(65534,
                dfhcommarea.getLsUnsignedNative().getLsFullwords().getLsP9X9Low());
        assertEquals(4294967295L,
                dfhcommarea.getLsUnsignedNative().getLsFullwords().getLsP9X9Max());
        assertEquals(0,
                dfhcommarea.getLsUnsignedNative().getLsFullwords().getLsP9X9Min());

        assertEquals(new BigInteger("18446744069414584318"),
                dfhcommarea.getLsUnsignedNative().getLsDoublewords().getLsP9X18High());
        assertEquals(new BigInteger("4294967294"),
                dfhcommarea.getLsUnsignedNative().getLsDoublewords().getLsP9X18Low());
        assertEquals(new BigInteger("18446744073709551615"),
                dfhcommarea.getLsUnsignedNative().getLsDoublewords().getLsP9X18Max());
        assertEquals(new BigInteger("0"),
                dfhcommarea.getLsUnsignedNative().getLsDoublewords().getLsP9X18Min());
     }
    
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "0000"
        + "007f"
        + "8001"
        + "ffff"
        + "00000000"
        + "0000fffe"
        + "80000001"
        + "ffffffff"
        + "0000000000000000"
        + "00000000fffffffe"
        + "fffffffefffffffe"
        + "ffffffffffffffff";
    }

    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }

}
