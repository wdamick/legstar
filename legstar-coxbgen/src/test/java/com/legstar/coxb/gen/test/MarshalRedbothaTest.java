/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.coxb.gen.test;


import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolMarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.host.HostData;
import com.legstar.host.HostException;
import com.legstar.test.coxb.redbotha.Filler22Type;

import junit.framework.TestCase;

public class MarshalRedbothaTest extends TestCase {

	public void testRedbotha() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[2];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.redbotha.ObjectFactory objectFactory = new com.legstar.test.coxb.redbotha.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.redbotha.DfhcommareaType dfhcommareaType = objectFactory.createDfhcommareaType();
		dfhcommareaType.setCNumeric(5);

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.redbotha.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.redbotha.bind.DfhcommareaTypeBinding(objectFactory, dfhcommareaType);
		ccem.accept(mv);
		assertEquals("0005",HostData.toHexString(hostBytes));
	}

	public void testRedbothaSecondChoice() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[2];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.redbotha.ObjectFactory objectFactory = new com.legstar.test.coxb.redbotha.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.redbotha.DfhcommareaType dfhcommareaType = objectFactory.createDfhcommareaType();
		Filler22Type filler22 = objectFactory.createFiller22Type();
		filler22.setCLeftByte("A");
		filler22.setCRightByte("B");
		dfhcommareaType.setFiller22(filler22);

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.redbotha.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.redbotha.bind.DfhcommareaTypeBinding(objectFactory, dfhcommareaType);
		ccem.accept(mv);
		assertEquals("c1c2",HostData.toHexString(hostBytes));
	}
}
