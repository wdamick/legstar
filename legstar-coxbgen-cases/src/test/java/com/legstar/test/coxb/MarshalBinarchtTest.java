/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.test.coxb;



import java.math.BigInteger;

import com.legstar.test.coxb.binarcht.LsSignedNative;
import com.legstar.test.coxb.binarcht.LsUnsignedNative;
import com.legstar.test.coxb.binarcht.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal binarcht.
 *
 */
public class MarshalBinarchtTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "binarcht";

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testBinarcht() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);
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

        assertEquals("0000"
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
        + "002bdc545d6b4b87",
                Util.marshal(SCHEMA_NAME, dfhcommarea, 56));
    }
}
