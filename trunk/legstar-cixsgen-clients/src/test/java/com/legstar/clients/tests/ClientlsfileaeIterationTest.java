/*******************************************************************************
 * Copyright (c) 2009 LegSem.
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

public class ClientlsfileaeIterationTest extends TestCase {
	
	private final static int ITERATIONS = 10;
	
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
		req.setDfhcommarea(dfhcommarea);
		
		LsfileaeHostHeader reqHead = wsOF.createLsfileaeHostHeader();
		reqHead.setHostUserID("P390");
		reqHead.setHostPassword("STREAM2");
		reqHead.setHostEndPoint("CICSTS23DirectHttp");
		
		dfhcommarea.setComNumber(100);

		for (int i = 0; i < ITERATIONS; i++) {
			LsfileaeResponse resp = port.lsfileae(req, reqHead);
			Dfhcommarea dfhcommareaResp = resp.getDfhcommarea();

			assertEquals("SURREY, ENGLAND",dfhcommareaResp.getComPersonal().getComAddress());
			assertEquals("$0100.11",dfhcommareaResp.getComAmount());
			assertEquals("26 11 81",dfhcommareaResp.getComDate());
			assertEquals("S. D. BORMAN",dfhcommareaResp.getComPersonal().getComName());
			assertEquals(100,dfhcommareaResp.getComNumber());
			assertEquals("32156778",dfhcommareaResp.getComPersonal().getComPhone());
			assertEquals("*********",dfhcommareaResp.getComComment());
		}
	}

}
