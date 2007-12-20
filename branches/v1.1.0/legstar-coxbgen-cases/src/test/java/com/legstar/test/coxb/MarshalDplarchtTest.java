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

import com.legstar.test.coxb.dplarcht.DfhcommareaType;
import com.legstar.test.coxb.dplarcht.LsRequestType;

import junit.framework.TestCase;

public class MarshalDplarchtTest extends TestCase {

	private final static String SCHEMA_NAME = "dplarcht";

	public void testDplarcht() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		LsRequestType lsRequestType = new LsRequestType();
		lsRequestType.setLsRequestType(0);
		lsRequestType.setLsAllItems("*");

		dfhcommareaType.setLsRequest(lsRequestType);

		assertEquals("00005c4040404040404040404040000000000f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 88));
	}
}
