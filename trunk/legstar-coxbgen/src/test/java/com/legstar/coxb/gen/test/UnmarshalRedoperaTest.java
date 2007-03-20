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
import com.legstar.coxb.CobolUnmarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;

public class UnmarshalRedoperaTest extends TestCase {

	public void testRedoperaStringMethod() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		//		           <------------------------------------>
		//		            1 2 3 4 5 6 7 8 9 101112131415161718
		//		            s t r i n g M e t h o d             A B J A D H A O U A Z    
		String hexString = "a2a399899587d485a3889684404040404040c1c2d1c1c4c8c1d6e4c1e9404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.redopera.ObjectFactory objectFactory = new com.legstar.test.coxb.redopera.ObjectFactory();

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.redopera.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.redopera.bind.DfhcommareaTypeBinding(objectFactory);
		ccem.accept(uv);
		com.legstar.test.coxb.redopera.DfhcommareaType dfhcommareaType = ccem.getJaxbObject();
		
		assertEquals("ABJADHAOUAZ                   ", dfhcommareaType.getFiller25().getCString());
	}

	public void testRedoperaIntMethod() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		//		           <------------------------------------>
		//		            1 2 3 4 5 6 7 8 9 101112131415161718
		//		            i n t M e t h o d                   0 0 0 0 0 3 4 5    
		String hexString = "8995a3d485a3889684404040404040404040f0f0f0f0f0f3f4f5404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.redopera.ObjectFactory objectFactory = new com.legstar.test.coxb.redopera.ObjectFactory();

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.redopera.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.redopera.bind.DfhcommareaTypeBinding(objectFactory);
		ccem.accept(uv);
		com.legstar.test.coxb.redopera.DfhcommareaType dfhcommareaType = ccem.getJaxbObject();
		
		assertEquals(null, dfhcommareaType.getFiller25());
		assertEquals(345, dfhcommareaType.getFiller28().getCInteger());
	}
}
