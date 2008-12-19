/*******************************************************************************
 * Copyright (c) 2008 LegSem.
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
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

public class MarshalSmallVolumeTest extends TestCase {

	private static int ITERATIONS = 100;
	
	public void testMarshal() throws HostException {
		byte[] hostBytes = new byte[25];
		
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(new CobolContext());
		// Create a concrete visitor
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.dplarcht.Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
		
		com.legstar.test.coxb.dplarcht.LsRequest lsRequest = objectFactory.createLsRequest();
		dfhcommarea.setLsRequest(lsRequest);
		lsRequest.setLsRequestType(1); // request programs
		lsRequest.setLsAllItems("*");  // no limit to number of items
		com.legstar.test.coxb.dplarcht.LsSearchCriteria lsSearchCriteria = objectFactory.createLsSearchCriteria();
		lsSearchCriteria.setLsStartwith("C");
		lsSearchCriteria.setLsStartwithLen(1);
		lsRequest.setLsSearchCriteria(lsSearchCriteria);
		
		com.legstar.test.coxb.dplarcht.LsReply lsReply = objectFactory.createLsReply();
		dfhcommarea.setLsReply(lsReply);
		com.legstar.test.coxb.dplarcht.LsReplyData lsReplyData = objectFactory.createLsReplyData();
		lsReply.setLsReplyData(lsReplyData);
		lsReplyData.setLsItemsCount(0);
		
		// Perform mashaling a number of times
		for (int i = 0; i < ITERATIONS; i++) {
            // Traverse the object structure, visiting each node with the visitor
			CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
            
			mv.setOffset(0);
			ccem.accept(mv);
		}
           assertEquals(25, mv.getOffset());
	}
}
