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

public class UnmarshalBinarchtTest extends TestCase {

	public void testBinarcht() throws Exception {

		//		              <--><--><------><------><--------------><--------------><--><--><------><------><--------------><-------------->
		//		              1 2 1 2 1 2 3 4 1 2 3 4 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 1 2 1 2 3 4 1 2 3 4 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 
		//		                 06553       042949672                1844674407370955-3273276-123456721474836-1234567890123451234567890123456         
		String hexString   = "0000ffff00000000ffffffff0000000000000000ffffffffffffffff80007ffff8a432eb7fffffffffd423aba294b479002bdc545d6b4b87";
		byte[] hostBytes = HostData.toByteArray(hexString);

		Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "binarcht");
		
		assertEquals(12345678901234567l, Dfhcommarea.getLsSignedNative().getLsPs9X18Max());
		assertEquals(-12345678901234567l, Dfhcommarea.getLsSignedNative().getLsPs9X18Min());
		assertEquals(32767, Dfhcommarea.getLsSignedNative().getLsPs9X4Max());
		assertEquals(-32768, Dfhcommarea.getLsSignedNative().getLsPs9X4Min());
		assertEquals(2147483647, Dfhcommarea.getLsSignedNative().getLsPs9X9Max());
		assertEquals(-123456789, Dfhcommarea.getLsSignedNative().getLsPs9X9Min());
		
		assertEquals(65535, Dfhcommarea.getLsUnsignedNative().getLsP9X4Max());
		assertEquals(0, Dfhcommarea.getLsUnsignedNative().getLsP9X4Min());
		assertEquals(4294967295l, Dfhcommarea.getLsUnsignedNative().getLsP9X9Max());
		assertEquals(0l, Dfhcommarea.getLsUnsignedNative().getLsP9X9Min());
		assertEquals(new BigInteger("18446744073709551615"), Dfhcommarea.getLsUnsignedNative().getLsP9X18Max());
		assertEquals(new BigInteger("0"), Dfhcommarea.getLsUnsignedNative().getLsP9X18Min());
	}
}
