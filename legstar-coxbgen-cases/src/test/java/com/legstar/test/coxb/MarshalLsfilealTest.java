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

/**
 * Marshal lsfileal.
 *
 */
public class MarshalLsfilealTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "lsfileal";

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
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

        assertEquals("0000"
        + "f0f07af0f57af2f2"
        + "000000044f"
        + "4040404040404040404040"
        + "4040404040404040404040"
        + "4040404040404040404040"
        + "4040404040404040404040"
        + "4040404040404040404040"
        + "4040404040404040404040"
        + "4040404040404040404040"
        + "4040404040404040404040"
        + "4040404040404040404040"
        + "4040404040404040404040"
        + "4040404040404040404040"
        + "4040"
        + "000000002f"

        + "f0f0f0f2f0f3"
        + "c2d6d9c9e2404040404040404040404040404040"
        + "d1d6c9d5e5c9d3d3c54040404040404040404040"
        + "f0f1f2f5f4f6f8f9"
        + "f2f3f0f4f5f94040"
        + "5bf2f5f04bf1f240"
        + "e8c5d540c140d7c1e2"

        + "f0f0f0f2f0f4"
        + "c3c8d9c9e2e3c9c1d54040404040404040404040"
        + "d5d6c7c5d5e34040404040404040404040404040"
        + "f2f4f9f6f5f8f4f7"
        + "f2f3f4f5f8f94040"
        + "5bf0f4f54bf7f840"
        + "e2c940e8c5d540c140",
                Util.marshal(SCHEMA_NAME, "ReplyData", replyData, 301));
    }

    /**
     * Marshal host data and test java data object result.
     * Error case.
     * @throws Exception if marshaling fails
     */
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

        assertEquals("0001"
        + "00000013"
        + "00000050"
        + "c6c9d3c540c3d3d6e2c5"
        + "c4404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "40404040404040404040"
        + "4040404040404040"
        + "000000000f",
                Util.marshal(SCHEMA_NAME, "ReplyData", replyData, 143));
    }
}
