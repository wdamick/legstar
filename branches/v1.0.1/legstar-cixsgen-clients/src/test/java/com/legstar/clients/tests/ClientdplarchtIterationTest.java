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

import javax.xml.namespace.QName;

import junit.framework.TestCase;
import com.legstar.test.cixs.dplarcht.*;
import com.legstar.test.coxb.dplarcht.*;

import java.net.MalformedURLException;
import java.net.URL;

public class ClientdplarchtIterationTest extends TestCase {
	
	private final static int ITERATIONS = 10;
	
	public void testPrograms() throws DplarchtFault, MalformedURLException{
		com.legstar.test.cixs.dplarcht.ObjectFactory wsOF =
		    new com.legstar.test.cixs.dplarcht.ObjectFactory();
		com.legstar.test.coxb.dplarcht.ObjectFactory obOF =
		    new com.legstar.test.coxb.dplarcht.ObjectFactory();
		DplarchtService sv = new DplarchtService(
				new URL("http://localhost:8080/cixs-dplarcht/dplarcht?wsdl"),
				new QName("http://cixs.test.legstar.com/dplarcht", "dplarchtService"));
		DplarchtPort port = sv.getDplarchtImplPort();
		
		
		DplarchtRequest req = wsOF.createDplarchtRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		LsRequestType lsRequestType = obOF.createLsRequestType();
		lsRequestType.setLsRequestType(1);
		lsRequestType.setLsAllItems("*");
		
		dfhcommarea.setLsRequest(lsRequestType);
		
		for (int i = 0; i < ITERATIONS; i++) {
			DplarchtResponse resp = port.dplarcht(req, null);
			DfhcommareaType dfhcommareaResp = resp.getResponse();
			
			assertEquals(500,dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsCount());
			assertEquals("LE370       ",dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramLanguage());
			assertEquals(0,dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramLength());
			assertEquals("AALPHBET",dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramName());
			assertEquals("PROGRAM     ",dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramType());
			assertEquals("                        ",dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getFiller113());
			assertEquals(null,dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsFilesData());
			assertEquals(null,dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsTransactionsData());
		}
	}

}
