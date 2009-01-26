/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.impl.reflect.perf.test;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.visitor.CobolUnmarshalVisitor;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

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
			com.legstar.test.coxb.dplarcht.Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
			
			// Traverse the object structure, visiting each node with the visitor
			CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
			mv.setOffset(0);
			ccem.accept(mv);
			assertEquals(1, dfhcommarea.getLsRequest().getLsRequestType());
			assertEquals("*", dfhcommarea.getLsRequest().getLsAllItems()); 
			assertEquals("C", dfhcommarea.getLsRequest().getLsSearchCriteria().getLsStartwith());
			assertEquals(1, dfhcommarea.getLsRequest().getLsSearchCriteria().getLsStartwithLen());
			assertEquals(0, dfhcommarea.getLsReply().getLsReplyType());
			assertEquals(0, dfhcommarea.getLsReply().getLsReplyData().getLsItemsCount());
		}
           assertEquals(25, mv.getOffset());
	}
}
