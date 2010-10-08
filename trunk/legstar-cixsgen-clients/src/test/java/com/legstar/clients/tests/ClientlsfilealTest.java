/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
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
		
		LsfilealPort port = sv.getLsfilealPort();
		
		Map <String, Object > requestContext = ((BindingProvider)port).getRequestContext();
		requestContext.put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY,"http://localhost:8080/cixs-lsfileal/lsfileal");
//		requestContext.put(BindingProvider.USERNAME_PROPERTY, "enduser");
//		requestContext.put(BindingProvider.PASSWORD_PROPERTY, "tomcat");
		
		LsfilealRequest req = wsOF.createLsfilealRequest();
		RequestParms requestParms = obOF.createRequestParms();
		req.setRequestParms(requestParms);
		
		requestParms.setRequestName("S*");
		LsfilealHostHeader reqHead = wsOF.createLsfilealHostHeader();
		reqHead.setHostEndPoint("CICSTS23DirectHttp");

		LsfilealResponse resp = port.lsfileal(req, reqHead);
		ReplyData replyData = resp.getReplyData();
		
		assertEquals(0, replyData.getReplyType());
		assertEquals(null, replyData.getReplyErrorHeader());
		assertEquals(43, replyData.getReplySuccessHeader().getTotalItemsRead());
		assertTrue(replyData.getReplySuccessHeader().getSearchDuration().contains("00:00:"));
		assertEquals(5, replyData.getFiller65().getReplyItemscount());
		ReplyItem replyItem = replyData.getFiller65().getReplyItem().get(0);

		assertEquals("SURREY, ENGLAND",replyItem.getReplyPersonal().getReplyAddress());
		assertEquals("$0100.11",replyItem.getReplyAmount());
		assertEquals("26 11 81",replyItem.getReplyDate());
		assertEquals("S. D. BORMAN",replyItem.getReplyPersonal().getReplyName());
		assertEquals(100,replyItem.getReplyNumber());
		assertEquals("32156778",replyItem.getReplyPersonal().getReplyPhone());
		assertEquals("*********",replyItem.getReplyComment());
	}
}
