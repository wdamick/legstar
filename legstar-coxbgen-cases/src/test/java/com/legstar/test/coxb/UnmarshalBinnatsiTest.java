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



import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.binnatsi.Dfhcommarea;

import junit.framework.TestCase;

public class UnmarshalBinnatsiTest extends TestCase {

	public void testBinnatsi() throws Exception {

		String hexString   = "8000ff8004157fff80000000ffffff80075bcd157fffffff8000000000000000ffffffff0000000200000003ffffffff7fffffffffffffff";
		byte[] hostBytes = HostData.toByteArray(hexString);

		Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "binnatsi");
		
		assertEquals(1045, Dfhcommarea.getLsUnsignedNative().getLsHalfwords().getLsPs9X4High());
		assertEquals(-128, Dfhcommarea.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Low());
		assertEquals(32767, Dfhcommarea.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Max());
		assertEquals(-32768, Dfhcommarea.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Min());
		
		assertEquals(123456789, Dfhcommarea.getLsUnsignedNative().getLsFullwords().getLsPs9X9High());
		assertEquals(-128, Dfhcommarea.getLsUnsignedNative().getLsFullwords().getLsPs9X9Low());
		assertEquals(2147483647, Dfhcommarea.getLsUnsignedNative().getLsFullwords().getLsPs9X9Max());
		assertEquals(-2147483648, Dfhcommarea.getLsUnsignedNative().getLsFullwords().getLsPs9X9Min());
		
		assertEquals(17179869183l, Dfhcommarea.getLsUnsignedNative().getLsDoublewords().getLsPs9X18High());
		assertEquals(-4294967294l, Dfhcommarea.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Low());
		assertEquals(9223372036854775807l, Dfhcommarea.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Max());
		assertEquals(-9223372036854775808l, Dfhcommarea.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Min());
	}
}
