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
import com.legstar.test.cixs.lsfileae.*;
import com.legstar.test.coxb.lsfileae.*;
import javax.xml.ws.BindingProvider;
import java.util.Map;

public class ClientlsfileaeTest extends TestCase {
	
	public void testClientNullHeader() throws LsfileaeFault{
		com.legstar.test.cixs.lsfileae.ObjectFactory wsOF =
		    new com.legstar.test.cixs.lsfileae.ObjectFactory();
		com.legstar.test.coxb.lsfileae.ObjectFactory obOF =
		    new com.legstar.test.coxb.lsfileae.ObjectFactory();
		
		LsfileaeService sv = new LsfileaeService();
		
		LsfileaePort port = sv.getLsfileaeImplPort();
		
		Map <String, Object > requestContext = ((BindingProvider)port).getRequestContext();
		requestContext.put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY,"http://localhost:8080/cixs-lsfileae/lsfileae");
		requestContext.put(BindingProvider.USERNAME_PROPERTY, "enduser");
		requestContext.put(BindingProvider.PASSWORD_PROPERTY, "tomcat");
		
		LsfileaeRequest req = wsOF.createLsfileaeRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setComNumber(100);

		LsfileaeResponse resp = port.lsfileae(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();

		assertEquals("SURREY, ENGLAND     ",dfhcommareaResp.getComPersonal().getComAddress());
		assertEquals("$0100.11",dfhcommareaResp.getComAmount());
		assertEquals("26 11 81",dfhcommareaResp.getComDate());
		assertEquals("S. D. BORMAN        ",dfhcommareaResp.getComPersonal().getComName());
		assertEquals(100,dfhcommareaResp.getComNumber());
		assertEquals("32156778",dfhcommareaResp.getComPersonal().getComPhone());
		assertEquals("*********",dfhcommareaResp.getComComment());
	}

	public void testClientHeaderUserIDPasswordCorrect() throws LsfileaeFault{
		com.legstar.test.cixs.lsfileae.ObjectFactory wsOF =
		    new com.legstar.test.cixs.lsfileae.ObjectFactory();
		com.legstar.test.coxb.lsfileae.ObjectFactory obOF =
		    new com.legstar.test.coxb.lsfileae.ObjectFactory();
		
		LsfileaeService sv = new LsfileaeService();
		
		LsfileaePort port = sv.getLsfileaeImplPort();
		
		Map <String, Object > requestContext = ((BindingProvider)port).getRequestContext();
		requestContext.put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY,"http://localhost:8080/cixs-lsfileae/lsfileae");
		requestContext.put(BindingProvider.USERNAME_PROPERTY, "enduser");
		requestContext.put(BindingProvider.PASSWORD_PROPERTY, "tomcat");
		
		LsfileaeHostHeader reqHead = wsOF.createLsfileaeHostHeader();
		reqHead.setHostUserID("P390");
		reqHead.setHostPassword("STREAM2");
		
		LsfileaeRequest req = wsOF.createLsfileaeRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setComNumber(100);

		LsfileaeResponse resp = port.lsfileae(req, reqHead);
		DfhcommareaType dfhcommareaResp = resp.getResponse();

		assertEquals("SURREY, ENGLAND     ",dfhcommareaResp.getComPersonal().getComAddress());
		assertEquals("$0100.11",dfhcommareaResp.getComAmount());
		assertEquals("26 11 81",dfhcommareaResp.getComDate());
		assertEquals("S. D. BORMAN        ",dfhcommareaResp.getComPersonal().getComName());
		assertEquals(100,dfhcommareaResp.getComNumber());
		assertEquals("32156778",dfhcommareaResp.getComPersonal().getComPhone());
		assertEquals("*********",dfhcommareaResp.getComComment());
	}

	public void testClientHeaderUserIDPasswordIncorrect() throws LsfileaeFault{
		com.legstar.test.cixs.lsfileae.ObjectFactory wsOF =
		    new com.legstar.test.cixs.lsfileae.ObjectFactory();
		com.legstar.test.coxb.lsfileae.ObjectFactory obOF =
		    new com.legstar.test.coxb.lsfileae.ObjectFactory();
		
		LsfileaeService sv = new LsfileaeService();
		
		LsfileaePort port = sv.getLsfileaeImplPort();
		
		Map <String, Object > requestContext = ((BindingProvider)port).getRequestContext();
		requestContext.put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY,"http://localhost:8080/cixs-lsfileae/lsfileae");
		requestContext.put(BindingProvider.USERNAME_PROPERTY, "enduser");
		requestContext.put(BindingProvider.PASSWORD_PROPERTY, "tomcat");
		
		LsfileaeHostHeader reqHead = wsOF.createLsfileaeHostHeader();
		reqHead.setHostUserID("TOZ");
		reqHead.setHostPassword("STREAM2");
		reqHead.setHostEndPoint("TheMainframeDirectHttp");
			
		LsfileaeRequest req = wsOF.createLsfileaeRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setComNumber(100);

		try {
			port.lsfileae(req, reqHead);
			fail("False header test failed");
		} catch (LsfileaeFault e) {
			assertTrue(e.getMessage().contains("Basic Authentication Error"));
		}
	}

	public void testClientHeaderCompleteFalse() throws LsfileaeFault{
		com.legstar.test.cixs.lsfileae.ObjectFactory wsOF =
		    new com.legstar.test.cixs.lsfileae.ObjectFactory();
		com.legstar.test.coxb.lsfileae.ObjectFactory obOF =
		    new com.legstar.test.coxb.lsfileae.ObjectFactory();
		
		LsfileaeService sv = new LsfileaeService();
		
		LsfileaePort port = sv.getLsfileaeImplPort();
		
		Map <String, Object > requestContext = ((BindingProvider)port).getRequestContext();
		requestContext.put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY,"http://localhost:8080/cixs-lsfileae/lsfileae");
		requestContext.put(BindingProvider.USERNAME_PROPERTY, "enduser");
		requestContext.put(BindingProvider.PASSWORD_PROPERTY, "tomcat");
		
		LsfileaeHostHeader reqHead = wsOF.createLsfileaeHostHeader();
		reqHead.setHostUserID("P390");
		reqHead.setHostPassword("STREAM2");
		reqHead.setHostEndPoint("nonExistantMainframe");
		
		LsfileaeRequest req = wsOF.createLsfileaeRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setComNumber(100);

		try {
			port.lsfileae(req, reqHead);
			fail("False header test failed");
		} catch (LsfileaeFault e) {
			assertEquals("Failed to invoke host program: org.apache.commons.configuration.ConfigurationException: The requested endpoint:nonExistantMainframe is not defined.",e.getMessage());
		}
		
	}

	public void testClientHeaderCompleteCorrect() throws LsfileaeFault{
		com.legstar.test.cixs.lsfileae.ObjectFactory wsOF =
		    new com.legstar.test.cixs.lsfileae.ObjectFactory();
		com.legstar.test.coxb.lsfileae.ObjectFactory obOF =
		    new com.legstar.test.coxb.lsfileae.ObjectFactory();
		
		LsfileaeService sv = new LsfileaeService();
		
		LsfileaePort port = sv.getLsfileaeImplPort();
		
		Map <String, Object > requestContext = ((BindingProvider)port).getRequestContext();
		requestContext.put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY,"http://localhost:8080/cixs-lsfileae/lsfileae");
		requestContext.put(BindingProvider.USERNAME_PROPERTY, "enduser");
		requestContext.put(BindingProvider.PASSWORD_PROPERTY, "tomcat");
		
		LsfileaeHostHeader reqHead = wsOF.createLsfileaeHostHeader();
		reqHead.setHostUserID("P390");
		reqHead.setHostPassword("STREAM2");
		reqHead.setHostEndPoint("TheMainframeDirectHttp");
		
		LsfileaeRequest req = wsOF.createLsfileaeRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setComNumber(100);

		try {
			LsfileaeResponse resp = port.lsfileae(req, reqHead);
			DfhcommareaType dfhcommareaResp = resp.getResponse();

			assertEquals("SURREY, ENGLAND     ",dfhcommareaResp.getComPersonal().getComAddress());
			assertEquals("$0100.11",dfhcommareaResp.getComAmount());
			assertEquals("26 11 81",dfhcommareaResp.getComDate());
			assertEquals("S. D. BORMAN        ",dfhcommareaResp.getComPersonal().getComName());
			assertEquals(100,dfhcommareaResp.getComNumber());
			assertEquals("32156778",dfhcommareaResp.getComPersonal().getComPhone());
			assertEquals("*********",dfhcommareaResp.getComComment());
		} catch (LsfileaeFault e) {
			fail(e.getMessage());
		}
		
	}
}
