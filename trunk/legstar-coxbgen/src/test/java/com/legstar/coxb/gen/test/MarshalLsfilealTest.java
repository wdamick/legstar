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

public class MarshalLsfilealTest extends TestCase {

	public void testLsfileal() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[301];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.lsfileal.ObjectFactory objectFactory = new com.legstar.test.coxb.lsfileal.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.lsfileal.ReplyDataType replyData = objectFactory.createReplyDataType();
		replyData.setReplyType(0); // Success

		com.legstar.test.coxb.lsfileal.ReplySuccessHeaderType replySuccessHeader = objectFactory.createReplySuccessHeaderType();
		replySuccessHeader.setSearchDuration("00:05:22");
		replySuccessHeader.setTotalItemsRead(44);
		replyData.setReplySuccessHeader(replySuccessHeader);
		com.legstar.test.coxb.lsfileal.Filler65Type replyFiller65 = objectFactory.createFiller65Type();
		
		com.legstar.test.coxb.lsfileal.ReplyItemType replyItem1 = objectFactory.createReplyItemType();
		com.legstar.test.coxb.lsfileal.ReplyPersonalType replyPersonal1 = objectFactory.createReplyPersonalType();
		replyPersonal1.setReplyAddress("JOINVILLE");
		replyPersonal1.setReplyName("BORIS");
		replyPersonal1.setReplyPhone("0125468975");
		replyItem1.setReplyNumber(203L);
		replyItem1.setReplyComment("YEN A PAS");
		replyItem1.setReplyDate("230459");
		replyItem1.setReplyPersonal(replyPersonal1);
		replyItem1.setReplyAmount("$250.12");
		replyFiller65.getReplyItem().add(replyItem1);
		
		com.legstar.test.coxb.lsfileal.ReplyItemType replyItem2 = objectFactory.createReplyItemType();
		com.legstar.test.coxb.lsfileal.ReplyPersonalType replyPersonal2 = objectFactory.createReplyPersonalType();
		replyPersonal2.setReplyAddress("NOGENT");
		replyPersonal2.setReplyName("CHRISTIAN");
		replyPersonal2.setReplyPhone("24965847");
		replyItem2.setReplyNumber(204L);
		replyItem2.setReplyComment("SI YEN A");
		replyItem2.setReplyDate("234589");
		replyItem2.setReplyPersonal(replyPersonal2);
		replyItem2.setReplyAmount("$045.78");
		replyFiller65.getReplyItem().add(replyItem2);
		
		replyFiller65.setReplyItemscount(2);
		replyData.setFiller65(replyFiller65);
		
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.lsfileal.bind.ReplyDataTypeBinding ccem = new com.legstar.test.coxb.lsfileal.bind.ReplyDataTypeBinding(objectFactory, replyData);
		ccem.accept(mv);
		assertEquals(301, mv.getOffset());
		assertEquals("0000f0f07af0f57af2f2000000044f404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040000000002ff0f0f0f2f0f3c2d6d9c9e2404040404040404040404040404040d1d6c9d5e5c9d3d3c54040404040404040404040f0f1f2f5f4f6f8f9f2f3f0f4f5f940405bf2f5f04bf1f240e8c5d540c140d7c1e2f0f0f0f2f0f4c3c8d9c9e2e3c9c1d54040404040404040404040d5d6c7c5d5e34040404040404040404040404040f2f4f9f6f5f8f4f7f2f3f4f5f8f940405bf0f4f54bf7f840e2c940e8c5d540c140",HostData.toHexString(hostBytes));
	}

	public void testLsfilealError() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[143];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.lsfileal.ObjectFactory objectFactory = new com.legstar.test.coxb.lsfileal.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.lsfileal.ReplyDataType replyData = objectFactory.createReplyDataType();
		replyData.setReplyType(1); // Error

		com.legstar.test.coxb.lsfileal.ReplyErrorHeaderType replyErrorHeader = objectFactory.createReplyErrorHeaderType();
		replyErrorHeader.setReplyResp(19);
		replyErrorHeader.setReplyResp2(80);
		replyErrorHeader.setReplyMessage("FILE CLOSED");
		replyData.setReplyErrorHeader(replyErrorHeader);
		
		com.legstar.test.coxb.lsfileal.Filler65Type replyFiller65 = objectFactory.createFiller65Type();
		replyFiller65.setReplyItemscount(0);
		
		replyData.setFiller65(replyFiller65);
		
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.lsfileal.bind.ReplyDataTypeBinding ccem = new com.legstar.test.coxb.lsfileal.bind.ReplyDataTypeBinding(objectFactory, replyData);
		ccem.accept(mv);
		assertEquals(143, mv.getOffset());
		assertEquals("00010000001300000050c6c9d3c540c3d3d6e2c5c4404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040000000000f",HostData.toHexString(hostBytes));
	}
}
