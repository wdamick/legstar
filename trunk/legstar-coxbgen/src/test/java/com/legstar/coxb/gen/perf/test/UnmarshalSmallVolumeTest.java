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
package com.legstar.coxb.gen.perf.test;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolUnmarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;

public class UnmarshalSmallVolumeTest extends TestCase {

	private static int ITERATIONS = 100;
	
	public void testUnmarshal() throws HostException {
		String hexString   = "00015c404040c340404040404040000000001f000000000000";
		byte[] hostBytes = HostData.toByteArray(hexString);

		
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(new CobolContext());
		// Create a concrete visitor
		CobolUnmarshalVisitor mv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();
		
		// Perform mashaling a number of times
		for (int i = 0; i < ITERATIONS; i++) {
			// Create and populate an instance of an object (JAXB annotated)
			com.legstar.test.coxb.dplarcht.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();
			
			// Traverse the object structure, visiting each node with the visitor
            com.legstar.test.coxb.dplarcht.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.dplarcht.bind.DfhcommareaTypeBinding(objectFactory, dfhcommarea);
			mv.setOffset(0);
			ccem.accept(mv);
		}
        assertEquals(25, mv.getOffset());
	}
}
