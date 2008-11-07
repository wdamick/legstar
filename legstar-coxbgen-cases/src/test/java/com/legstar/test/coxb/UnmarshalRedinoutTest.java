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
import com.legstar.test.coxb.redinout.Dfhcommarea;

import junit.framework.TestCase;

public class UnmarshalRedinoutTest extends TestCase {

	public void testRedinout() throws Exception {

		String hexString   = "0023f1f2f3f4f5f6f7f8c1c2c3c4c5c1c2c3c4c5c1c2c3c4c5d5c2";
		byte[] hostBytes = HostData.toByteArray(hexString);
		Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "redinout");
		
		assertEquals(35,Dfhcommarea.getCNumeric());
		assertEquals(12345678,Dfhcommarea.getCParaout().getCSomeOutput());
		assertEquals(null,Dfhcommarea.getCParain());
	}

}
