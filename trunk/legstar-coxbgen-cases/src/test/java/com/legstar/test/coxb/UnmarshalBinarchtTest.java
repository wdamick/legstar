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
import com.legstar.test.coxb.binarcht.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Unmarshal binarcht.
 *
 */
public class UnmarshalBinarchtTest extends TestCase {

    /**
     * Unmarshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testBinarcht() throws Exception {

        String hexString   = "0000"
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
        byte[] hostBytes = HostData.toByteArray(hexString);

        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "binarcht");

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
}
