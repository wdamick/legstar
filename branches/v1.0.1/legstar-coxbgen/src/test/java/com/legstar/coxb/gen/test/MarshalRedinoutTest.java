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

public class MarshalRedinoutTest extends TestCase {

	public void testRedinout() throws HostException{
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[17];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.redinout.ObjectFactory objectFactory = new com.legstar.test.coxb.redinout.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.redinout.DfhcommareaType dfhcommareaType = objectFactory.createDfhcommareaType();
		
		dfhcommareaType.setCNumeric(35);
		com.legstar.test.coxb.redinout.CParainType parain = objectFactory.createCParainType();
		parain.setCSomeInput("ABCDEABCDEABCDE");
		dfhcommareaType.setCParain(parain);

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.redinout.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.redinout.bind.DfhcommareaTypeBinding(objectFactory, dfhcommareaType);
		ccem.accept(mv);
		assertEquals("0023c1c2c3c4c5c1c2c3c4c5c1c2c3c4c5",HostData.toHexString(hostBytes));
	}

	public void testRedinoutByteLength() throws HostException{
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[17];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.redinout.ObjectFactory objectFactory = new com.legstar.test.coxb.redinout.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.redinout.DfhcommareaType dfhcommareaType = objectFactory.createDfhcommareaType();
		
		dfhcommareaType.setCNumeric(35);
		com.legstar.test.coxb.redinout.CParainType parain = objectFactory.createCParainType();
		parain.setCSomeInput("ABCDEABCDEABCDE");
		dfhcommareaType.setCParain(parain);

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.redinout.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.redinout.bind.DfhcommareaTypeBinding(objectFactory, dfhcommareaType);
		ccem.accept(mv);
		assertEquals(502,ccem.getByteLength());
	}

}
