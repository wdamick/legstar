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

import javax.xml.namespace.QName;

import junit.framework.TestCase;
import com.legstar.test.cixs.dplarcht.*;
import com.legstar.test.coxb.dplarcht.*;

import java.net.MalformedURLException;
import java.net.URL;

public class ClientdplarchtTest extends TestCase {
	
	public void testFiles() throws DplarchtFault{
		com.legstar.test.cixs.dplarcht.ObjectFactory wsOF =
		    new com.legstar.test.cixs.dplarcht.ObjectFactory();
		com.legstar.test.coxb.dplarcht.ObjectFactory obOF =
		    new com.legstar.test.coxb.dplarcht.ObjectFactory();
		DplarchtPort port = new DplarchtService().getDplarchtImplPort();
		
	
		DplarchtRequest req = wsOF.createDplarchtRequest();
		Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
		req.setRequest(dfhcommarea);
		
		LsRequest lsRequest = obOF.createLsRequest();
		lsRequest.setLsRequestType(0);
		lsRequest.setLsAllItems("*");
		
		dfhcommarea.setLsRequest(lsRequest);

		DplarchtHostHeader reqHead = wsOF.createDplarchtHostHeader();
		reqHead.setHostEndPoint("CICSTS23DirectHttp");
		
		DplarchtResponse resp = port.dplarcht(req, reqHead);
		Dfhcommarea dfhcommareaResp = resp.getResponse();
		
		assertEquals(0,dfhcommareaResp.getLsReply().getLsReplyType());
		assertEquals(5,dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsCount());
		assertEquals("CICSTS23.CICS.DFHCSD", dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsFilesData().getLsFileDsname().trim());
		assertEquals("", dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(1).getLsFilesData().getLsFileDsname().trim());
		assertEquals("CICSTS23.CICS.DFHLRQ", dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(2).getLsFilesData().getLsFileDsname().trim());
		assertEquals("CICSTS23.TCPIP.EZACONFG", dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(3).getLsFilesData().getLsFileDsname().trim());
		assertEquals("CICSTS23.CICS.FILEA", dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(4).getLsFilesData().getLsFileDsname().trim());
	}

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
		Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
		req.setRequest(dfhcommarea);
		
		LsRequest lsRequest = obOF.createLsRequest();
		lsRequest.setLsRequestType(1);
		lsRequest.setLsAllItems("*");
		
		dfhcommarea.setLsRequest(lsRequest);
		
		DplarchtHostHeader reqHead = wsOF.createDplarchtHostHeader();
		reqHead.setHostEndPoint("CICSTS23DirectHttp");
		
		DplarchtResponse resp = port.dplarcht(req, reqHead);
		Dfhcommarea dfhcommareaResp = resp.getResponse();
		
		assertEquals(500,dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsCount());
		assertEquals("LE370",dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramLanguage());
		assertEquals(0,dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramLength());
		assertEquals("AALPHBET",dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramName());
		assertEquals("PROGRAM",dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramType());
		assertEquals("",dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getFiller113());
		assertEquals(null,dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsFilesData());
		assertEquals(null,dfhcommareaResp.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsTransactionsData());
	}

}
