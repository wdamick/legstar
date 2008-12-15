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

public class UnmarshalBinnatusTest extends TestCase {

    public void testBinnatus() throws Exception {

        String hexString   = "0000007f8001ffff000000000000fffe80000001ffffffff000000000000000000000000fffffffefffffffefffffffeffffffffffffffff";
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "binnatus");

        assertEquals(32769, Dfhcommarea.getLsUnsignedNative().getLsHalfwords().getLsP9X4High());
        assertEquals(127, Dfhcommarea.getLsUnsignedNative().getLsHalfwords().getLsP9X4Low());
        assertEquals(65535, Dfhcommarea.getLsUnsignedNative().getLsHalfwords().getLsP9X4Max());
        assertEquals(0, Dfhcommarea.getLsUnsignedNative().getLsHalfwords().getLsP9X4Min());

        assertEquals(2147483649l, Dfhcommarea.getLsUnsignedNative().getLsFullwords().getLsP9X9High());
        assertEquals(65534, Dfhcommarea.getLsUnsignedNative().getLsFullwords().getLsP9X9Low());
        assertEquals(4294967295l, Dfhcommarea.getLsUnsignedNative().getLsFullwords().getLsP9X9Max());
        assertEquals(0, Dfhcommarea.getLsUnsignedNative().getLsFullwords().getLsP9X9Min());

        assertEquals(new BigInteger("18446744069414584318"), Dfhcommarea.getLsUnsignedNative().getLsDoublewords().getLsP9X18High());
        assertEquals(new BigInteger("4294967294"), Dfhcommarea.getLsUnsignedNative().getLsDoublewords().getLsP9X18Low());
        assertEquals(new BigInteger("18446744073709551615"), Dfhcommarea.getLsUnsignedNative().getLsDoublewords().getLsP9X18Max());
        assertEquals(new BigInteger("0"), Dfhcommarea.getLsUnsignedNative().getLsDoublewords().getLsP9X18Min());
    }
}
