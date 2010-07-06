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

import com.legstar.test.coxb.binnatsi.Dfhcommarea;
import com.legstar.test.coxb.binnatsi.LsDoublewords;
import com.legstar.test.coxb.binnatsi.LsFullwords;
import com.legstar.test.coxb.binnatsi.LsHalfwords;
import com.legstar.test.coxb.binnatsi.LsUnsignedNative;
import com.legstar.test.coxb.binnatsi.ObjectFactory;

import junit.framework.TestCase;


/**
 * Provides data samples for testing throughout LegStar.  
 */
public class BinnatsiCases extends TestCase {

    /** Utility class. */
    private BinnatsiCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();

        LsUnsignedNative lsUnsignedNative = new LsUnsignedNative(); 

        LsHalfwords lsHalfwords = new LsHalfwords();
        lsHalfwords.setLsPs9X4High(new Short("1045"));
        lsHalfwords.setLsPs9X4Low(new Short("-128"));
        lsHalfwords.setLsPs9X4Max(new Short("32767"));
        lsHalfwords.setLsPs9X4Min(new Short("-32768"));

        LsFullwords lsFullwords = new LsFullwords();
        lsFullwords.setLsPs9X9High(123456789);
        lsFullwords.setLsPs9X9Low(-128);
        lsFullwords.setLsPs9X9Max(2147483647);
        lsFullwords.setLsPs9X9Min(-2147483648);

        LsDoublewords lsDoublewords = new LsDoublewords();
        lsDoublewords.setLsPs9X18High(17179869183L);
        lsDoublewords.setLsPs9X18Low(-4294967294L);
        lsDoublewords.setLsPs9X18Max(9223372036854775807L);
        lsDoublewords.setLsPs9X18Min(-9223372036854775808L);

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
        assertEquals(1045,
                dfhcommarea.getLsUnsignedNative().getLsHalfwords().getLsPs9X4High());
        assertEquals(-128,
                dfhcommarea.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Low());
        assertEquals(32767,
                dfhcommarea.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Max());
        assertEquals(-32768,
                dfhcommarea.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Min());

        assertEquals(123456789,
                dfhcommarea.getLsUnsignedNative().getLsFullwords().getLsPs9X9High());
        assertEquals(-128,
                dfhcommarea.getLsUnsignedNative().getLsFullwords().getLsPs9X9Low());
        assertEquals(2147483647,
                dfhcommarea.getLsUnsignedNative().getLsFullwords().getLsPs9X9Max());
        assertEquals(-2147483648,
                dfhcommarea.getLsUnsignedNative().getLsFullwords().getLsPs9X9Min());

        assertEquals(17179869183L,
                dfhcommarea.getLsUnsignedNative().getLsDoublewords().getLsPs9X18High());
        assertEquals(-4294967294L,
                dfhcommarea.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Low());
        assertEquals(9223372036854775807L,
                dfhcommarea.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Max());
        assertEquals(-9223372036854775808L,
                dfhcommarea.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Min());
    }
    
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "8000"
        + "ff80"
        + "0415"
        + "7fff"
        + "80000000"
        + "ffffff80"
        + "075bcd15"
        + "7fffffff"
        + "8000000000000000"
        + "ffffffff00000002"
        + "00000003ffffffff"
        + "7fffffffffffffff";
    }

    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }

}
