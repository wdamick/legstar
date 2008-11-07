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

import junit.framework.TestCase;
import com.legstar.test.coxb.doublmix.Dfhcommarea;

public class UnmarshalDoublmixTest extends TestCase {

	public void testDoublmix() throws Exception {

		String hexString   = "434d2000000000000000000000000000411000000000000045543ae915b573e0361677a4590fab6060ffffff048ff9e0";
		byte[] hostBytes = HostData.toByteArray(hexString);
		Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "doublmix");
		
		assertEquals(0d,Dfhcommarea.getCDouble0());
		assertEquals(1d,Dfhcommarea.getCDouble1());
		assertEquals(1234d,Dfhcommarea.getCDouble1234());
		assertEquals(345006.56779999984d,Dfhcommarea.getCDouble345006P5678());
		assertEquals(3.4028234699999995E+38,Dfhcommarea.getCDouble3P40282347Ep38());
		assertEquals(7.982006699999985E-14,Dfhcommarea.getCDouble798P20067Em16());
	}
}
