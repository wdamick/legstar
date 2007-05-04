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
package com.legstar.coxb.gen.test;


import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolMarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;

public class MarshalRedmultiTest extends TestCase {

	public void testRedmultiNormal() throws HostException{
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[206];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.redmulti.ObjectFactory objectFactory = new com.legstar.test.coxb.redmulti.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.redmulti.DfhcommareaType dfhcommareaType = objectFactory.createDfhcommareaType();
		
		dfhcommareaType.setCOutputType("normal");
		com.legstar.test.coxb.redmulti.Filler35Type filler35Type = objectFactory.createFiller35Type();
		filler35Type.setCString("ABJADHAOUAZ");
		dfhcommareaType.setFiller35(filler35Type);

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.redmulti.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.redmulti.bind.DfhcommareaTypeBinding(objectFactory, dfhcommareaType);
		ccem.accept(mv);
		assertEquals("959699948193c1c2d1c1c4c8c1d6e4c1e9404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040", HostData.toHexString(hostBytes));
	}

	public void testRedmultiError() throws HostException{
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[206];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.redmulti.ObjectFactory objectFactory = new com.legstar.test.coxb.redmulti.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.redmulti.DfhcommareaType dfhcommareaType = objectFactory.createDfhcommareaType();
		
		dfhcommareaType.setCOutputType("error");
		com.legstar.test.coxb.redmulti.Filler38Type filler38Type = objectFactory.createFiller38Type();
		filler38Type.setCErrorNum(75);
		filler38Type.setCErrorDescription("ABOMINABLE");
		dfhcommareaType.setFiller38(filler38Type);

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.redmulti.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.redmulti.bind.DfhcommareaTypeBinding(objectFactory, dfhcommareaType);
		ccem.accept(mv);
		assertEquals("859999969940f0f0f7f5c1c2d6d4c9d5c1c2d3c5404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040", HostData.toHexString(hostBytes));
	}

}
