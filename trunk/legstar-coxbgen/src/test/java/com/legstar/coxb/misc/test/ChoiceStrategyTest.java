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
package com.legstar.coxb.misc.test;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolMarshalVisitor;
import com.legstar.coxb.CobolUnmarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.reflect.CComplexBinding;
import com.legstar.host.HostException;
import com.legstar.host.HostData;

import junit.framework.TestCase;

public class ChoiceStrategyTest extends TestCase {

	/* This test fakes a situation where marshalling will use a user written
	 *  choice selector in order to determine which alternative to use. */
	public void testDplarchtMarshalStatic() throws HostException {
		
		byte[] hostBytes = new byte[89];
		
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(new CobolContext());
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.dplarcht.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();

		com.legstar.test.coxb.dplarcht.LsRequestType lsRequestType = objectFactory.createLsRequestType();
		dfhcommarea.setLsRequest(lsRequestType);
		lsRequestType.setLsRequestType(1); // request programs
		lsRequestType.setLsAllItems("*");  // no limit to number of items
		com.legstar.test.coxb.dplarcht.LsSearchCriteriaType lsSearchCriteriaType = objectFactory.createLsSearchCriteriaType();
		lsSearchCriteriaType.setLsStartwith("C");
		lsSearchCriteriaType.setLsStartwithLen(1);
		lsRequestType.setLsSearchCriteria(lsSearchCriteriaType);

		com.legstar.test.coxb.dplarcht.LsReplyType lsReplyType = objectFactory.createLsReplyType();
		dfhcommarea.setLsReply(lsReplyType);
		com.legstar.test.coxb.dplarcht.LsReplyDataType lsReplyDataType = objectFactory.createLsReplyDataType();
		lsReplyType.setLsReplyData(lsReplyDataType);
		
		lsReplyDataType.setLsItemsCount(1); 
        com.legstar.test.coxb.dplarcht.LsItemsArrayType ia = objectFactory.createLsItemsArrayType();
        
        com.legstar.test.coxb.dplarcht.LsProgramsDataType pt = objectFactory.createLsProgramsDataType();
        pt.setLsProgramName("PROGRAM1");
        pt.setLsProgramLength(1750);
        pt.setLsProgramType("C");
        pt.setLsProgramLanguage("ASM");
        pt.setLsProgramUsecount(1);
        
        ia.setLsProgramsData(pt);
        lsReplyDataType.getLsItemsArray().add(ia);

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.dplarcht.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.dplarcht.bind.DfhcommareaTypeBinding(objectFactory, dfhcommarea);

		mv.setOffset(0);
		ccem.accept(mv);
		
        //            <--><------><-------------><---------><--><------><--------------><----------------------><----------------------><------><------><---------------------------------------------->
		//               1C       *                        1   0       1P R O G R A M 1 C                       A S M                       1750       1
		assertEquals("00015c404040c340404040404040000000001f000000000001d7d9d6c7d9c1d4f1c34040404040404040404040c1e2d4404040404040404040000006d600000001404040404040404040404040404040404040404040404040", HostData.toHexString(hostBytes));
	}
	
	public void testDplarchtUnmarshalStatic() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
        //                    <--><------><-------------><---------><--><------><--------------><----------------------><----------------------><------><------><---------------------------------------------->
		//                    1C       *                        1   0       1P R O G R A M 1 C                       A S M                       1750       1
		String hexString   = "00015c404040c340404040404040000000001f000000000001d7d9d6c7d9c1d4f1c34040404040404040404040c1e2d4404040404040404040000006d600000001404040404040404040404040404040404040404040404040";
		byte[] hostBytes = HostData.toByteArray(hexString);

		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.dplarcht.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.dplarcht.bind.DfhcommareaTypeBinding(objectFactory);
		ccem.accept(uv);
		com.legstar.test.coxb.dplarcht.DfhcommareaType dfhcommareaType = ccem.getJaxbObject();
	
		assertEquals(1,dfhcommareaType.getLsReply().getLsReplyData().getLsItemsCount());
		assertEquals("ASM         ",dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramLanguage());
		assertEquals(1750,dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramLength());
		assertEquals("PROGRAM1",dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramName());
		assertEquals("C           ",dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramType());
		assertEquals(1,dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramUsecount());
		assertEquals("                        ",dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getFiller113());
		assertEquals(null,dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsFilesData());
		assertEquals(null,dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsTransactionsData());
	}

	/* This test fakes a situation where marshalling will use a user written
	 *  choice selector in order to determine which alternative to use. */
	public void testDplarchtMarshalDynamic() throws HostException {
		
		byte[] hostBytes = new byte[89];
		
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(new CobolContext());
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.dplarcht.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();

		com.legstar.test.coxb.dplarcht.LsRequestType lsRequestType = objectFactory.createLsRequestType();
		dfhcommarea.setLsRequest(lsRequestType);
		lsRequestType.setLsRequestType(1); // request programs
		lsRequestType.setLsAllItems("*");  // no limit to number of items
		com.legstar.test.coxb.dplarcht.LsSearchCriteriaType lsSearchCriteriaType = objectFactory.createLsSearchCriteriaType();
		lsSearchCriteriaType.setLsStartwith("C");
		lsSearchCriteriaType.setLsStartwithLen(1);
		lsRequestType.setLsSearchCriteria(lsSearchCriteriaType);

		com.legstar.test.coxb.dplarcht.LsReplyType lsReplyType = objectFactory.createLsReplyType();
		dfhcommarea.setLsReply(lsReplyType);
		com.legstar.test.coxb.dplarcht.LsReplyDataType lsReplyDataType = objectFactory.createLsReplyDataType();
		lsReplyType.setLsReplyData(lsReplyDataType);
		
		lsReplyDataType.setLsItemsCount(1); 
        com.legstar.test.coxb.dplarcht.LsItemsArrayType ia = objectFactory.createLsItemsArrayType();
        
        com.legstar.test.coxb.dplarcht.LsProgramsDataType pt = objectFactory.createLsProgramsDataType();
        pt.setLsProgramName("PROGRAM1");
        pt.setLsProgramLength(1750);
        pt.setLsProgramType("C");
        pt.setLsProgramLanguage("ASM");
        pt.setLsProgramUsecount(1);
        
        ia.setLsProgramsData(pt);
        lsReplyDataType.getLsItemsArray().add(ia);

		// Traverse the object structure, visiting each node with the visitor
		CComplexBinding ccem = new CComplexBinding(objectFactory, dfhcommarea);

		mv.setOffset(0);
		ccem.accept(mv);
		
        //            <--><------><-------------><---------><--><------><--------------><----------------------><----------------------><------><------><---------------------------------------------->
		//               1C       *                        1   0       1P R O G R A M 1 C                       A S M                       1750       1
		assertEquals("00015c404040c340404040404040000000001f000000000001d7d9d6c7d9c1d4f1c34040404040404040404040c1e2d4404040404040404040000006d600000001404040404040404040404040404040404040404040404040", HostData.toHexString(hostBytes));
	}
	
	public void testDplarchtUnmarshalDynamic() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
        //                    <--><------><-------------><---------><--><------><--------------><----------------------><----------------------><------><------><---------------------------------------------->
		//                    1C       *                        1   0       1P R O G R A M 1 C                       A S M                       1750       1
		String hexString   = "00015c404040c340404040404040000000001f000000000001d7d9d6c7d9c1d4f1c34040404040404040404040c1e2d4404040404040404040000006d600000001404040404040404040404040404040404040404040404040";
		byte[] hostBytes = HostData.toByteArray(hexString);

		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.dplarcht.DfhcommareaType dfhcommareaType = objectFactory.createDfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexBinding ccem = new CComplexBinding(objectFactory, dfhcommareaType);
		ccem.accept(uv);
		
		assertEquals(1,dfhcommareaType.getLsReply().getLsReplyData().getLsItemsCount());
		assertEquals("ASM         ",dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramLanguage());
		assertEquals(1750,dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramLength());
		assertEquals("PROGRAM1",dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramName());
		assertEquals("C           ",dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramType());
		assertEquals(1,dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramUsecount());
		assertEquals("                        ",dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getFiller113());
		assertEquals(null,dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsFilesData().getLsFileName());
		assertEquals(null,dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsTransactionsData().getLsTransactionName());
	}
}
