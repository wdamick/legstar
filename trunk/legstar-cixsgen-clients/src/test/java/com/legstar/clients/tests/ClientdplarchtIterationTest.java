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

import javax.xml.namespace.QName;

import junit.framework.TestCase;
import com.legstar.test.cixs.dplarcht.*;
import com.legstar.test.coxb.dplarcht.*;

import java.net.MalformedURLException;
import java.net.URL;

public class ClientdplarchtIterationTest extends TestCase {

    private final static int ITERATIONS = 10;

    public void testPrograms() throws DplarchtFault, MalformedURLException {
        com.legstar.test.cixs.dplarcht.ObjectFactory wsOF =
                new com.legstar.test.cixs.dplarcht.ObjectFactory();
        com.legstar.test.coxb.dplarcht.ObjectFactory obOF =
                new com.legstar.test.coxb.dplarcht.ObjectFactory();
        DplarchtService sv = new DplarchtService(
                new URL("http://localhost:8080/cixs-dplarcht/dplarcht?wsdl"),
                new QName("http://cixs.test.legstar.com/dplarcht",
                        "dplarchtService"));
        DplarchtPort port = sv.getDplarchtPort();

        DplarchtRequest req = wsOF.createDplarchtRequest();
        Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
        req.setDfhcommarea(dfhcommarea);

        LsRequest lsRequest = obOF.createLsRequest();
        lsRequest.setLsRequestType(1);
        lsRequest.setLsAllItems("*");

        dfhcommarea.setLsRequest(lsRequest);
        DplarchtHostHeader reqHead = wsOF.createDplarchtHostHeader();
        reqHead.setHostEndPoint("CICSTS23DirectHttp");

        for (int i = 0; i < ITERATIONS; i++) {
            DplarchtResponse resp = port.dplarcht(req, reqHead);
            Dfhcommarea dfhcommareaResp = resp.getDfhcommarea();

            assertEquals(500, dfhcommareaResp.getLsReply().getLsReplyData()
                    .getLsItemsCount());
            assertEquals("LE370", dfhcommareaResp.getLsReply().getLsReplyData()
                    .getLsItemsArray().get(0).getLsProgramsData()
                    .getLsProgramLanguage());
            assertEquals(0, dfhcommareaResp.getLsReply().getLsReplyData()
                    .getLsItemsArray().get(0).getLsProgramsData()
                    .getLsProgramLength());
            assertEquals("AALPHBET", dfhcommareaResp.getLsReply()
                    .getLsReplyData().getLsItemsArray().get(0)
                    .getLsProgramsData().getLsProgramName());
            assertEquals("PROGRAM", dfhcommareaResp.getLsReply()
                    .getLsReplyData().getLsItemsArray().get(0)
                    .getLsProgramsData().getLsProgramType());
            assertEquals("", dfhcommareaResp.getLsReply().getLsReplyData()
                    .getLsItemsArray().get(0).getLsProgramsData()
                    .getFiller113());
            assertEquals(null, dfhcommareaResp.getLsReply().getLsReplyData()
                    .getLsItemsArray().get(0).getLsFilesData());
            assertEquals(null, dfhcommareaResp.getLsReply().getLsReplyData()
                    .getLsItemsArray().get(0).getLsTransactionsData());
        }
    }

}
