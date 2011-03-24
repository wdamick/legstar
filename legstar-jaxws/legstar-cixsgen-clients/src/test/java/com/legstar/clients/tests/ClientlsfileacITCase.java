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

import java.util.Map;

import javax.xml.ws.BindingProvider;

import com.legstar.test.cixs.lsfileac.LsfileacFault;
import com.legstar.test.cixs.lsfileac.LsfileacHostHeader;
import com.legstar.test.cixs.lsfileac.LsfileacPort;
import com.legstar.test.cixs.lsfileac.LsfileacRequest;
import com.legstar.test.cixs.lsfileac.LsfileacRequestHolder;
import com.legstar.test.cixs.lsfileac.LsfileacResponse;
import com.legstar.test.cixs.lsfileac.LsfileacResponseHolder;
import com.legstar.test.cixs.lsfileac.LsfileacService;
import com.legstar.test.coxb.lsfileac.QueryData;
import com.legstar.test.coxb.lsfileac.QueryLimit;

public class ClientlsfileacITCase extends AbstractITCase {

    public void testClientHeaderCompleteCorrect() throws LsfileacFault {
        com.legstar.test.cixs.lsfileac.ObjectFactory wsOF = new com.legstar.test.cixs.lsfileac.ObjectFactory();
        com.legstar.test.coxb.lsfileac.ObjectFactory obOF = new com.legstar.test.coxb.lsfileac.ObjectFactory();

        LsfileacService sv = new LsfileacService();

        LsfileacPort port = sv.getLsfileacPort();

        Map < String, Object > requestContext = ((BindingProvider) port)
                .getRequestContext();
        requestContext.put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY,
                "http://localhost:8080/cixs-lsfileac/lsfileac");
        requestContext.put(BindingProvider.USERNAME_PROPERTY, "enduser");
        requestContext.put(BindingProvider.PASSWORD_PROPERTY, "tomcat");

        LsfileacHostHeader reqHead = wsOF.createLsfileacHostHeader();
        reqHead.setHostUserID("P390");
        reqHead.setHostPassword("STREAM2");
        reqHead.setHostEndPoint("CICSTS31DirectHttp");

        QueryData qdt = obOF.createQueryData();
        qdt.setQueryAddress("*");
        qdt.setQueryName("S*");
        qdt.setQueryPhone("*");

        QueryLimit qlt = obOF.createQueryLimit();
        qlt.setMaxElapseTime(5000);
        qlt.setMaxItemsRead(100);

        LsfileacRequestHolder reqHolder = wsOF.createLsfileacRequestHolder();
        reqHolder.setQueryData(qdt);
        reqHolder.setQueryLimit(qlt);

        LsfileacRequest req = wsOF.createLsfileacRequest();
        req.setLsfileacRequestHolder(reqHolder);

        try {
            LsfileacResponse resp = port.lsfileac(req, reqHead);
            LsfileacResponseHolder respHolder = resp
                    .getLsfileacResponseHolder();
            assertEquals(43, respHolder.getReplyStatus().getTotalItemsRead());
            assertEquals("", respHolder.getReplyStatus().getReplyMessage()
                    .trim());
            assertEquals(0, respHolder.getReplyStatus().getReplyResp());
            assertEquals(0, respHolder.getReplyStatus().getReplyResp2());
            assertEquals(0, respHolder.getReplyStatus().getReplyType());
            assertEquals("00:00:00", respHolder.getReplyStatus()
                    .getSearchDuration());

            assertEquals(5, respHolder.getReplyData().getReplyItemscount());
            assertEquals(100, respHolder.getReplyData().getReplyItem().get(0)
                    .getReplyNumber());
            assertEquals("$0100.11", respHolder.getReplyData().getReplyItem()
                    .get(0).getReplyAmount());
            assertEquals("*********", respHolder.getReplyData().getReplyItem()
                    .get(0).getReplyComment());
            assertEquals("26 11 81", respHolder.getReplyData().getReplyItem()
                    .get(0).getReplyDate());
            assertEquals("SURREY, ENGLAND", respHolder.getReplyData()
                    .getReplyItem().get(0).getReplyPersonal().getReplyAddress());
            assertEquals("S. D. BORMAN", respHolder.getReplyData()
                    .getReplyItem().get(0).getReplyPersonal().getReplyName());
            assertEquals("32156778", respHolder.getReplyData().getReplyItem()
                    .get(0).getReplyPersonal().getReplyPhone());
        } catch (LsfileacFault e) {
            fail(e.getMessage());
        }

    }
}
