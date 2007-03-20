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

import junit.framework.TestCase;

public class MarshalRedsimptTest extends TestCase {

	public void testRedsimpt() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[18];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory = new com.legstar.test.coxb.redsimpt.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.redsimpt.DfhcommareaType dfhcommareaType = objectFactory.createDfhcommareaType();
		dfhcommareaType.setCDefinition1("ABCDEFGHIJKLMNO");

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.redsimpt.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.redsimpt.bind.DfhcommareaTypeBinding(objectFactory, dfhcommareaType);
		ccem.accept(mv);
		//		     <------------------------------------>
		//		      1 2 3 4 5 6 7 8 9 101112131415161718
		//		      A B C D E F G H I J K L M N O       
		assertEquals("c1c2c3c4c5c6c7c8c9d1d2d3d4d5d6404040",HostData.toHexString(hostBytes));
	}

	public void testRedsimptSecondChoice() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[18];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory = new com.legstar.test.coxb.redsimpt.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.redsimpt.DfhcommareaType dfhcommareaType = objectFactory.createDfhcommareaType();
		dfhcommareaType.setCDefinition2(123456789012345l);

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.redsimpt.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.redsimpt.bind.DfhcommareaTypeBinding(objectFactory, dfhcommareaType);
		ccem.accept(mv);
		//		     <------------------------------------>
		//		      1 2 3 4 5 6 7 8 9 101112131415161718
		//		      0 0 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5        
		assertEquals("f0f0f0f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5",HostData.toHexString(hostBytes));
	}
}
