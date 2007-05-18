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

package com.legstar.clients.tests;

import junit.framework.TestCase;

import com.legstar.test.cixs.lsfileal.*;
import com.legstar.test.coxb.lsfileal.*;
import javax.xml.ws.BindingProvider;
import java.util.Map;

public class ClientlsfilealTest extends TestCase {
	
	public void testClientNullHeader() throws LsfilealFault{
		com.legstar.test.cixs.lsfileal.ObjectFactory wsOF =
		    new com.legstar.test.cixs.lsfileal.ObjectFactory();
		com.legstar.test.coxb.lsfileal.ObjectFactory obOF =
		    new com.legstar.test.coxb.lsfileal.ObjectFactory();
		
		LsfilealService sv = new LsfilealService();
		
		LsfilealPort port = sv.getLsfilealImplPort();
		
		Map <String, Object > requestContext = ((BindingProvider)port).getRequestContext();
		requestContext.put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY,"http://localhost:8080/cixs-lsfileal/lsfileal");
//		requestContext.put(BindingProvider.USERNAME_PROPERTY, "enduser");
//		requestContext.put(BindingProvider.PASSWORD_PROPERTY, "tomcat");
		
		LsfilealRequest req = wsOF.createLsfilealRequest();
		RequestParmsType requestParms = obOF.createRequestParmsType();
		req.setRequest(requestParms);
		
		requestParms.setRequestName("S*");
		LsfilealHostHeader reqHead = wsOF.createLsfilealHostHeader();
		reqHead.setHostEndPoint("CICSTS23DirectHttp");

		LsfilealResponse resp = port.lsfileal(req, reqHead);
		ReplyDataType replyData = resp.getResponse();
		
		assertEquals(0, replyData.getReplyType());
		assertEquals(null, replyData.getReplyErrorHeader());
		assertEquals(45,replyData.getReplySuccessHeader().getTotalItemsRead());
		assertEquals("00:00:00",replyData.getReplySuccessHeader().getSearchDuration());
		assertEquals(5, replyData.getFiller65().getReplyItemscount());
		ReplyItemType replyItem = replyData.getFiller65().getReplyItem().get(0);

		assertEquals("SURREY, ENGLAND     ",replyItem.getReplyPersonal().getReplyAddress());
		assertEquals("$0100.11",replyItem.getReplyAmount());
		assertEquals("26 11 81",replyItem.getReplyDate());
		assertEquals("S. D. BORMAN        ",replyItem.getReplyPersonal().getReplyName());
		assertEquals(100,replyItem.getReplyNumber());
		assertEquals("32156778",replyItem.getReplyPersonal().getReplyPhone());
		assertEquals("*********",replyItem.getReplyComment());
	}
}
