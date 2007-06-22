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
package com.legstar.coxb.impl.reflect.perf.test;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolMarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.host.HostException;

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
		com.legstar.test.coxb.dplarcht.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();
		
		com.legstar.test.coxb.dplarcht.LsRequestType lsRequestType = objectFactory.createLsRequestType();
		dfhcommarea.setLsRequest(lsRequestType);
		lsRequestType.setLsRequestType(0); // request files
		lsRequestType.setLsAllItems("*");  // no limit to number of items
		com.legstar.test.coxb.dplarcht.LsSearchCriteriaType lsSearchCriteriaType = objectFactory.createLsSearchCriteriaType();
		lsSearchCriteriaType.setLsStartwith("C");
		lsSearchCriteriaType.setLsStartwithLen(1);
		lsRequestType.setLsSearchCriteria(lsSearchCriteriaType);
		
		com.legstar.test.coxb.dplarcht.LsReplyType lsReplyType = objectFactory.createLsReplyType();
		dfhcommarea.setLsReply(lsReplyType);
		com.legstar.test.coxb.dplarcht.LsReplyDataType lsReplyDataType = objectFactory.createLsReplyDataType();
		lsReplyType.setLsReplyData(lsReplyDataType);
		lsReplyDataType.setLsItemsCount(100);
        
        for (int i=0; i < 100; i++) {
            com.legstar.test.coxb.dplarcht.LsItemsArrayType ia = objectFactory.createLsItemsArrayType();
            com.legstar.test.coxb.dplarcht.LsFilesDataType dt = objectFactory.createLsFilesDataType();
            dt.setLsFileName("FILE" + (new Integer(i)).toString());
            dt.setLsFileDsname("this.is.file." + (new Integer(i)).toString());
            dt.setLsFileEnablestatus("ENABLED");
            ia.setLsFilesData(dt);
            lsReplyDataType.getLsItemsArray().add(ia);
        }
        
 		
		// Perform mashaling a number of times
		for (int i = 0; i < ITERATIONS; i++) {
            // Traverse the object structure, visiting each node with the visitor
			CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
            
			mv.setOffset(0);
			ccem.accept(mv);
		}
         assertEquals(6425, mv.getOffset());

	}
}
