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
import com.legstar.test.coxb.numzoned.DfhcommareaType;

import junit.framework.TestCase;

public class UnmarshalNumzonedTest extends TestCase {

	public void testNumzoned() throws Exception {

		//		            <><--><----><--><--><---->
		//		            1 1 2 1 2 3 1 2 1 2 1 2 3  
		//		            6   -5 -7 8   +1 + 9 1 1 - 
		String hexString = "f6f0d5d0f7f8f0c14ef9f1f160";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "numzoned");
		
		assertEquals(6, dfhcommareaType.getLU());
		assertEquals(-5, dfhcommareaType.getLS());
		assertEquals(-78, dfhcommareaType.getLSSignL());
		assertEquals(1, dfhcommareaType.getLSSignT());
		assertEquals(9, dfhcommareaType.getLSSignSL());
		assertEquals(-11, dfhcommareaType.getLSSignST());
	}
}
