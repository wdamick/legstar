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

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.binnatus.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Unmarshal binnatus.
 *
 */
public class UnmarshalBinnatusTest extends TestCase {

    /**
     * Unmarshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testBinnatus() throws Exception {

        String hexString   = "0000"
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
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "binnatus");

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
}
