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

import com.legstar.test.coxb.redmulti.DfhcommareaType;
import com.legstar.test.coxb.redmulti.Filler35Type;
import com.legstar.test.coxb.redmulti.Filler38Type;

import junit.framework.TestCase;

public class MarshalRedmultiTest extends TestCase {

	private final static String SCHEMA_NAME = "redmulti";
	
	public void testRedmultiNormal() throws Exception{
		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		
		dfhcommareaType.setCOutputType("normal");
		Filler35Type filler35Type = new Filler35Type();
		filler35Type.setCString("ABJADHAOUAZ");
		dfhcommareaType.setFiller35(filler35Type);

		assertEquals("959699948193c1c2d1c1c4c8c1d6e4c1e9404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 206));
	}

	public void testRedmultiError() throws Exception{
		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		
		dfhcommareaType.setCOutputType("error");
		Filler38Type filler38Type = new Filler38Type();
		filler38Type.setCErrorNum(75);
		filler38Type.setCErrorDescription("ABOMINABLE");
		dfhcommareaType.setFiller38(filler38Type);

		assertEquals("859999969940f0f0f7f5c1c2d6d4c9d5c1c2d3c5404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 206));
	}

}
