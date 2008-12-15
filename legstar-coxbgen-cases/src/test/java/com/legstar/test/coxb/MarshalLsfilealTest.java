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

import com.legstar.test.coxb.lsfileal.ReplyData;
import com.legstar.test.coxb.lsfileal.ReplySuccessHeader;
import com.legstar.test.coxb.lsfileal.Filler65;
import com.legstar.test.coxb.lsfileal.ReplyItem;
import com.legstar.test.coxb.lsfileal.ReplyPersonal;
import com.legstar.test.coxb.lsfileal.ReplyErrorHeader;

import junit.framework.TestCase;

public class MarshalLsfilealTest extends TestCase {

    private final static String SCHEMA_NAME = "lsfileal";

    public void testLsfileal() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        ReplyData replyData = (ReplyData) Util.getJaxbObject(SCHEMA_NAME, "ReplyData");
        replyData.setReplyType(0); // Success

        ReplySuccessHeader replySuccessHeader = new ReplySuccessHeader();
        replySuccessHeader.setSearchDuration("00:05:22");
        replySuccessHeader.setTotalItemsRead(44);
        replyData.setReplySuccessHeader(replySuccessHeader);
        Filler65 replyFiller65 = new Filler65();

        ReplyItem replyItem1 = new ReplyItem();
        ReplyPersonal replyPersonal1 = new ReplyPersonal();
        replyPersonal1.setReplyAddress("JOINVILLE");
        replyPersonal1.setReplyName("BORIS");
        replyPersonal1.setReplyPhone("0125468975");
        replyItem1.setReplyNumber(203L);
        replyItem1.setReplyComment("YEN A PAS");
        replyItem1.setReplyDate("230459");
        replyItem1.setReplyPersonal(replyPersonal1);
        replyItem1.setReplyAmount("$250.12");
        replyFiller65.getReplyItem().add(replyItem1);

        ReplyItem replyItem2 = new ReplyItem();
        ReplyPersonal replyPersonal2 = new ReplyPersonal();
        replyPersonal2.setReplyAddress("NOGENT");
        replyPersonal2.setReplyName("CHRISTIAN");
        replyPersonal2.setReplyPhone("24965847");
        replyItem2.setReplyNumber(204L);
        replyItem2.setReplyComment("SI YEN A");
        replyItem2.setReplyDate("234589");
        replyItem2.setReplyPersonal(replyPersonal2);
        replyItem2.setReplyAmount("$045.78");
        replyFiller65.getReplyItem().add(replyItem2);

        replyFiller65.setReplyItemscount(2);
        replyData.setFiller65(replyFiller65);

        assertEquals("0000f0f07af0f57af2f2000000044f404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040000000002ff0f0f0f2f0f3c2d6d9c9e2404040404040404040404040404040d1d6c9d5e5c9d3d3c54040404040404040404040f0f1f2f5f4f6f8f9f2f3f0f4f5f940405bf2f5f04bf1f240e8c5d540c140d7c1e2f0f0f0f2f0f4c3c8d9c9e2e3c9c1d54040404040404040404040d5d6c7c5d5e34040404040404040404040404040f2f4f9f6f5f8f4f7f2f3f4f5f8f940405bf0f4f54bf7f840e2c940e8c5d540c140",
                Util.marshal(SCHEMA_NAME, "ReplyData", replyData, 301));
    }

    public void testLsfilealError() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        ReplyData replyData = (ReplyData) Util.getJaxbObject(SCHEMA_NAME, "ReplyData");
        replyData.setReplyType(1); // Error

        ReplyErrorHeader replyErrorHeader = new ReplyErrorHeader();
        replyErrorHeader.setReplyResp(19);
        replyErrorHeader.setReplyResp2(80);
        replyErrorHeader.setReplyMessage("FILE CLOSED");
        replyData.setReplyErrorHeader(replyErrorHeader);

        Filler65 replyFiller65 = new Filler65();
        replyFiller65.setReplyItemscount(0);

        replyData.setFiller65(replyFiller65);

        assertEquals("00010000001300000050c6c9d3c540c3d3d6e2c5c4404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040000000000f",
                Util.marshal(SCHEMA_NAME, "ReplyData", replyData, 143));
    }
}
