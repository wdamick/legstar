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

import com.legstar.test.coxb.binnatus.LsUnsignedNative;
import com.legstar.test.coxb.binnatus.LsDoublewords;
import com.legstar.test.coxb.binnatus.LsFullwords;
import com.legstar.test.coxb.binnatus.LsHalfwords;
import com.legstar.test.coxb.binnatus.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal binnatus.
 *
 */
public class MarshalBinnatusTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "binnatus";

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testBinnatus() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);

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

        assertEquals("0000"
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
        + "ffffffffffffffff",
                Util.marshal(SCHEMA_NAME, dfhcommarea, 56));
    }
}
