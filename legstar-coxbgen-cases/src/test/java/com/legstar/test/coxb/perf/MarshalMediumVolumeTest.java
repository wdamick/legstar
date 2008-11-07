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
package com.legstar.test.coxb.perf;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.visitor.CobolMarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

public class MarshalMediumVolumeTest extends TestCase {

	private static int ITERATIONS = 100;
	
	public void testMarshal() throws HostException {
		byte[] hostBytes = new byte[6425];
		
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(new CobolContext());
		// Create a concrete visitor
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.dplarcht.Dfhcommarea Dfhcommarea = objectFactory.createDfhcommarea();
		
		com.legstar.test.coxb.dplarcht.LsRequest lsRequest = objectFactory.createLsRequest();
		Dfhcommarea.setLsRequest(lsRequest);
		lsRequest.setLsRequestType(0); // request files
		lsRequest.setLsAllItems("*");  // no limit to number of items
		com.legstar.test.coxb.dplarcht.LsSearchCriteria lsSearchCriteria = objectFactory.createLsSearchCriteria();
		lsSearchCriteria.setLsStartwith("C");
		lsSearchCriteria.setLsStartwithLen(1);
		lsRequest.setLsSearchCriteria(lsSearchCriteria);
		
		com.legstar.test.coxb.dplarcht.LsReply lsReply = objectFactory.createLsReply();
		Dfhcommarea.setLsReply(lsReply);
		com.legstar.test.coxb.dplarcht.LsReplyData lsReplyData = objectFactory.createLsReplyData();
		lsReply.setLsReplyData(lsReplyData);
		lsReplyData.setLsItemsCount(100);
        
        for (int i=0; i < 100; i++) {
            com.legstar.test.coxb.dplarcht.LsItemsArray ia = objectFactory.createLsItemsArray();
            com.legstar.test.coxb.dplarcht.LsFilesData dt = objectFactory.createLsFilesData();
            dt.setLsFileName("FILE" + (new Integer(i)).toString());
            dt.setLsFileDsname("this.is.file." + (new Integer(i)).toString());
            dt.setLsFileEnablestatus("ENABLED");
            ia.setLsFilesData(dt);
            lsReplyData.getLsItemsArray().add(ia);
        }
        
 		
		// Perform mashaling a number of times
		for (int i = 0; i < ITERATIONS; i++) {
            // Traverse the object structure, visiting each node with the visitor
            com.legstar.test.coxb.dplarcht.bind.DfhcommareaBinding ccem = new com.legstar.test.coxb.dplarcht.bind.DfhcommareaBinding(Dfhcommarea);
            
			mv.setOffset(0);
			ccem.accept(mv);
		}
        assertEquals(6425, mv.getOffset());

	}
}
