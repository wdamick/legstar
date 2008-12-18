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
import com.legstar.test.coxb.binpkdus.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Unmarshal binpkdus.
 *
 */
public class UnmarshalBinpkdusTest extends TestCase {

    /**
     * Unmarshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testBinpkdus() throws Exception {

        String hexString   = "0f"
            + "3f"
            + "012f"
            + "0032769f"
            + "0123456789012345678f"
            + "1234567890123456789f"
            + "1234567890123456789012345678901f"
            + "000000000000000000000000";
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "binpkdus");

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
}
