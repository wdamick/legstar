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

import junit.framework.TestCase;

import com.legstar.test.coxb.binpkdus.LsCompat;
import com.legstar.test.coxb.binpkdus.LsExtend;
import com.legstar.test.coxb.binpkdus.LsUnsignedPackedDecimal;
import com.legstar.test.coxb.binpkdus.ObjectFactory;
import com.legstar.test.coxb.binpkdus.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class BinpkdusCases extends TestCase {

    /** Utility class. */
    private BinpkdusCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();

        LsCompat lsCompat = new LsCompat();
        lsCompat.setLsP9X1Null(0);
        lsCompat.setLsP9X1(3);
        lsCompat.setLsP9X2(12);
        lsCompat.setLsP9X7(32769);
        lsCompat.setLsP9X18(123456789012345678L);

        LsExtend lsExtend = new LsExtend();
        lsExtend.setLsP9X19(new BigInteger("1234567890123456789"));
        lsExtend.setLsP9X31(new BigInteger("1234567890123456789012345678901"));

        LsUnsignedPackedDecimal lsUnsignedPackedDecimal = new LsUnsignedPackedDecimal();
        lsUnsignedPackedDecimal.setLsCompat(lsCompat);
        lsUnsignedPackedDecimal.setLsExtend(lsExtend);
        dfhcommarea.setLsUnsignedPackedDecimal(lsUnsignedPackedDecimal);

        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        assertEquals(3,
                dfhcommarea.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X1());
        assertEquals(123456789012345678L,
                dfhcommarea.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X18());
        assertEquals(0,
                dfhcommarea.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X1Null());
        assertEquals(12,
                dfhcommarea.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X2());
        assertEquals(32769,
                dfhcommarea.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X7());
        assertEquals(new BigInteger("1234567890123456789"),
                dfhcommarea.getLsUnsignedPackedDecimal().getLsExtend().getLsP9X19());
        assertEquals(new BigInteger("1234567890123456789012345678901"),
                dfhcommarea.getLsUnsignedPackedDecimal().getLsExtend().getLsP9X31());
    }
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "0f"
        + "3f"
        + "012f"
        + "0032769f"
        + "0123456789012345678f"
        + "1234567890123456789f"
        + "1234567890123456789012345678901f";
    }
}
