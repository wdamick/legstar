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
package com.legstar.test.coxb;



import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.lsfileal.ReplyDataType;

import junit.framework.TestCase;

public class UnmarshalLsfilealTest extends TestCase {

	public void testLsfileal() throws Exception {

		String hexString = "0000f0f07af0f57af2f2000000044f404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040000000002ff0f0f0f2f0f3c2d6d9c9e2404040404040404040404040404040d1d6c9d5e5c9d3d3c54040404040404040404040f0f1f2f5f4f6f8f9f2f3f0f4f5f940405bf2f5f04bf1f240e8c5d540c140d7c1e2f0f0f0f2f0f4c3c8d9c9e2e3c9c1d54040404040404040404040d5d6c7c5d5e34040404040404040404040404040f2f4f9f6f5f8f4f7f2f3f4f5f8f940405bf0f4f54bf7f840e2c940e8c5d540c140";
		byte[] hostBytes = HostData.toByteArray(hexString);
		ReplyDataType replyData = (ReplyDataType) Util.unmarshal(hostBytes, "lsfileal", "ReplyDataType");
		
		assertEquals(0,replyData.getReplyType());
		assertEquals(null,replyData.getReplyErrorHeader());
		assertEquals("00:05:22",replyData.getReplySuccessHeader().getSearchDuration());
		assertEquals(44,replyData.getReplySuccessHeader().getTotalItemsRead());
		assertEquals(2,replyData.getFiller65().getReplyItemscount());
		
		assertEquals(203,replyData.getFiller65().getReplyItem().get(0).getReplyNumber());
		assertEquals("$250.12",replyData.getFiller65().getReplyItem().get(0).getReplyAmount());
		assertEquals("YEN A PAS",replyData.getFiller65().getReplyItem().get(0).getReplyComment());
		assertEquals("230459",replyData.getFiller65().getReplyItem().get(0).getReplyDate());
		assertEquals("JOINVILLE",replyData.getFiller65().getReplyItem().get(0).getReplyPersonal().getReplyAddress());
		assertEquals("BORIS",replyData.getFiller65().getReplyItem().get(0).getReplyPersonal().getReplyName());
		assertEquals("01254689",replyData.getFiller65().getReplyItem().get(0).getReplyPersonal().getReplyPhone());
		
		assertEquals(204,replyData.getFiller65().getReplyItem().get(1).getReplyNumber());
		assertEquals("$045.78",replyData.getFiller65().getReplyItem().get(1).getReplyAmount());
		assertEquals("SI YEN A",replyData.getFiller65().getReplyItem().get(1).getReplyComment());
		assertEquals("234589",replyData.getFiller65().getReplyItem().get(1).getReplyDate());
		assertEquals("NOGENT",replyData.getFiller65().getReplyItem().get(1).getReplyPersonal().getReplyAddress());
		assertEquals("CHRISTIAN",replyData.getFiller65().getReplyItem().get(1).getReplyPersonal().getReplyName());
		assertEquals("24965847",replyData.getFiller65().getReplyItem().get(1).getReplyPersonal().getReplyPhone());
	}

	public void testLsfilealWithError() throws Exception {

		String hexString = "00010000001300000050c6c9d3c540c3d3d6e2c5c4404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040000000000f";
		byte[] hostBytes = HostData.toByteArray(hexString);
		ReplyDataType replyData = (ReplyDataType) Util.unmarshal(hostBytes, "lsfileal", "ReplyDataType");
		
		assertEquals(1,replyData.getReplyType());
		assertEquals(null,replyData.getReplySuccessHeader());
		assertEquals(19,replyData.getReplyErrorHeader().getReplyResp());
		assertEquals(80,replyData.getReplyErrorHeader().getReplyResp2());
		assertEquals("FILE CLOSED",replyData.getReplyErrorHeader().getReplyMessage().trim());
		assertEquals(0,replyData.getFiller65().getReplyItemscount());
	}
}
