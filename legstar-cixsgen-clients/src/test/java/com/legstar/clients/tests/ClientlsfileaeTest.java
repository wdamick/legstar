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
		Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setComNumber(100);

		LsfileaeResponse resp = port.lsfileae(req, null);
		Dfhcommarea dfhcommareaResp = resp.getResponse();

		assertEquals("SURREY, ENGLAND",dfhcommareaResp.getComPersonal().getComAddress());
		assertEquals("$0100.11",dfhcommareaResp.getComAmount());
		assertEquals("26 11 81",dfhcommareaResp.getComDate());
		assertEquals("S. D. BORMAN",dfhcommareaResp.getComPersonal().getComName());
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
		Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setComNumber(100);

		LsfileaeResponse resp = port.lsfileae(req, reqHead);
		Dfhcommarea dfhcommareaResp = resp.getResponse();

		assertEquals("SURREY, ENGLAND",dfhcommareaResp.getComPersonal().getComAddress());
		assertEquals("$0100.11",dfhcommareaResp.getComAmount());
		assertEquals("26 11 81",dfhcommareaResp.getComDate());
		assertEquals("S. D. BORMAN",dfhcommareaResp.getComPersonal().getComName());
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
		reqHead.setHostEndPoint("CICSTS23DirectHttp");
			
		LsfileaeRequest req = wsOF.createLsfileaeRequest();
		Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
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
		Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
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
		reqHead.setHostEndPoint("CICSTS23DirectHttp");
		
		LsfileaeRequest req = wsOF.createLsfileaeRequest();
		Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setComNumber(100);

		try {
			LsfileaeResponse resp = port.lsfileae(req, reqHead);
			Dfhcommarea dfhcommareaResp = resp.getResponse();

			assertEquals("SURREY, ENGLAND",dfhcommareaResp.getComPersonal().getComAddress());
			assertEquals("$0100.11",dfhcommareaResp.getComAmount());
			assertEquals("26 11 81",dfhcommareaResp.getComDate());
			assertEquals("S. D. BORMAN",dfhcommareaResp.getComPersonal().getComName());
			assertEquals(100,dfhcommareaResp.getComNumber());
			assertEquals("32156778",dfhcommareaResp.getComPersonal().getComPhone());
			assertEquals("*********",dfhcommareaResp.getComComment());
		} catch (LsfileaeFault e) {
			fail(e.getMessage());
		}
		
	}
	public void testClientDirectMQ() throws LsfileaeFault{
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
		reqHead.setHostEndPoint("CICSTS23DirectMQ");
		
		LsfileaeRequest req = wsOF.createLsfileaeRequest();
		Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setComNumber(100);

		try {
			LsfileaeResponse resp = port.lsfileae(req, reqHead);
			Dfhcommarea dfhcommareaResp = resp.getResponse();

			assertEquals("SURREY, ENGLAND",dfhcommareaResp.getComPersonal().getComAddress());
			assertEquals("$0100.11",dfhcommareaResp.getComAmount());
			assertEquals("26 11 81",dfhcommareaResp.getComDate());
			assertEquals("S. D. BORMAN",dfhcommareaResp.getComPersonal().getComName());
			assertEquals(100,dfhcommareaResp.getComNumber());
			assertEquals("32156778",dfhcommareaResp.getComPersonal().getComPhone());
			assertEquals("*********",dfhcommareaResp.getComComment());
		} catch (LsfileaeFault e) {
			fail(e.getMessage());
		}
		
	}
}
