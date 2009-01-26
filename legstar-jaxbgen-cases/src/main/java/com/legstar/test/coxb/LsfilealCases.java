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
package com.legstar.test.coxb;

import junit.framework.TestCase;

import com.legstar.test.coxb.lsfileal.Filler65;
import com.legstar.test.coxb.lsfileal.ObjectFactory;
import com.legstar.test.coxb.lsfileal.ReplyData;
import com.legstar.test.coxb.lsfileal.ReplyErrorHeader;
import com.legstar.test.coxb.lsfileal.ReplyItem;
import com.legstar.test.coxb.lsfileal.ReplyPersonal;
import com.legstar.test.coxb.lsfileal.ReplySuccessHeader;
import com.legstar.test.coxb.lsfileal.RequestParms;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class LsfilealCases extends TestCase {

    /** Utility class. */
    private LsfilealCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static ReplyData getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        ReplyData replyData = of.createReplyData();
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
        return replyData;
    }

    /**
     * Check that data object contains the expected values.
     * @param replyData the java object to check
     */
    public static void checkJavaObject(final ReplyData replyData) {
        assertEquals(0, replyData.getReplyType());
        assertEquals(null, replyData.getReplyErrorHeader());
        assertEquals("00:05:22", replyData.getReplySuccessHeader().getSearchDuration());
        assertEquals(44, replyData.getReplySuccessHeader().getTotalItemsRead());
        assertEquals(2, replyData.getFiller65().getReplyItemscount());

        assertEquals(203, replyData.getFiller65().getReplyItem().get(0).getReplyNumber());
        assertEquals("$250.12", replyData.getFiller65().getReplyItem().get(0).getReplyAmount());
        assertEquals("YEN A PAS", replyData.getFiller65().getReplyItem().get(0).getReplyComment());
        assertEquals("230459", replyData.getFiller65().getReplyItem().get(0).getReplyDate());
        assertEquals("JOINVILLE", replyData.getFiller65().getReplyItem().get(0).getReplyPersonal().getReplyAddress());
        assertEquals("BORIS", replyData.getFiller65().getReplyItem().get(0).getReplyPersonal().getReplyName());
        assertEquals("01254689", replyData.getFiller65().getReplyItem().get(0).getReplyPersonal().getReplyPhone());

        assertEquals(204, replyData.getFiller65().getReplyItem().get(1).getReplyNumber());
        assertEquals("$045.78", replyData.getFiller65().getReplyItem().get(1).getReplyAmount());
        assertEquals("SI YEN A", replyData.getFiller65().getReplyItem().get(1).getReplyComment());
        assertEquals("234589", replyData.getFiller65().getReplyItem().get(1).getReplyDate());
        assertEquals("NOGENT", replyData.getFiller65().getReplyItem().get(1).getReplyPersonal().getReplyAddress());
        assertEquals("CHRISTIAN", replyData.getFiller65().getReplyItem().get(1).getReplyPersonal().getReplyName());
        assertEquals("24965847", replyData.getFiller65().getReplyItem().get(1).getReplyPersonal().getReplyPhone());
    }
    /**
     * @return an instance of a valued java object.
     */
    public static ReplyData getJavaObjectError() {
        ObjectFactory of = new ObjectFactory();
        ReplyData replyData = of.createReplyData();
        replyData.setReplyType(1); // Error

        ReplyErrorHeader replyErrorHeader = new ReplyErrorHeader();
        replyErrorHeader.setReplyResp(19);
        replyErrorHeader.setReplyResp2(80);
        replyErrorHeader.setReplyMessage("FILE CLOSED");
        replyData.setReplyErrorHeader(replyErrorHeader);

        Filler65 replyFiller65 = new Filler65();
        replyFiller65.setReplyItemscount(0);

        replyData.setFiller65(replyFiller65);
        return replyData;
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "0000"
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
        + "e2c940e8c5d540c140";
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexError() { 

        return "0001"
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
        + "000000000f";
    }
    /**
     * Check that data object contains the expected values.
     * @param replyData the java object to check
     */
    public static void checkJavaObjectHexError(final ReplyData replyData) {
        assertEquals(1, replyData.getReplyType());
        assertEquals(null, replyData.getReplySuccessHeader());
        assertEquals(19, replyData.getReplyErrorHeader().getReplyResp());
        assertEquals(80, replyData.getReplyErrorHeader().getReplyResp2());
        assertEquals("FILE CLOSED", replyData.getReplyErrorHeader().getReplyMessage().trim());
        assertEquals(0, replyData.getFiller65().getReplyItemscount());
    }

    /**
     * @return an instance of a valued java object.
     */
    public static RequestParms getJavaObjectRequestSStar() {
        ObjectFactory of = new ObjectFactory();
        RequestParms requestParms = of.createRequestParms();
        requestParms.setRequestName("S*");
        return requestParms;
    }
    /**
     * Check that data object contains the expected values.
     * @param replyData the java object to check
     */
    public static void checkJavaObjectReeplySStar(final ReplyData replyData) {
        assertEquals(0, replyData.getReplyType());
        assertEquals(null, replyData.getReplyErrorHeader());
        assertEquals("00:00:00", replyData.getReplySuccessHeader().getSearchDuration());
        assertEquals(45, replyData.getReplySuccessHeader().getTotalItemsRead());
        assertEquals(5, replyData.getFiller65().getReplyItemscount());

        assertEquals(100, replyData.getFiller65().getReplyItem().get(0).getReplyNumber());
        assertEquals("$0100.11", replyData.getFiller65().getReplyItem().get(0).getReplyAmount());
        assertEquals("*********", replyData.getFiller65().getReplyItem().get(0).getReplyComment());
        assertEquals("26 11 81", replyData.getFiller65().getReplyItem().get(0).getReplyDate());
        assertEquals("SURREY, ENGLAND",
                replyData.getFiller65().getReplyItem().get(0).getReplyPersonal().getReplyAddress());
        assertEquals("S. D. BORMAN", replyData.getFiller65().getReplyItem().get(0).getReplyPersonal().getReplyName());
        assertEquals("32156778", replyData.getFiller65().getReplyItem().get(0).getReplyPersonal().getReplyPhone());

        assertEquals(762, replyData.getFiller65().getReplyItem().get(1).getReplyNumber());
        assertEquals("$0000.00", replyData.getFiller65().getReplyItem().get(1).getReplyAmount());
        assertEquals("*********", replyData.getFiller65().getReplyItem().get(1).getReplyComment());
        assertEquals("01 06 74", replyData.getFiller65().getReplyItem().get(1).getReplyDate());
        assertEquals("SAN JOSE,CALIFORNIA",
                replyData.getFiller65().getReplyItem().get(1).getReplyPersonal().getReplyAddress());
        assertEquals("SUSAN MALAIKA", replyData.getFiller65().getReplyItem().get(1).getReplyPersonal().getReplyName());
        assertEquals("22312121", replyData.getFiller65().getReplyItem().get(1).getReplyPersonal().getReplyPhone());

        assertEquals(6016, replyData.getFiller65().getReplyItem().get(2).getReplyNumber());
        assertEquals("$0009.88", replyData.getFiller65().getReplyItem().get(2).getReplyAmount());
        assertEquals("*********", replyData.getFiller65().getReplyItem().get(2).getReplyComment());
        assertEquals("21 05 74", replyData.getFiller65().getReplyItem().get(2).getReplyDate());
        assertEquals("NEW DELHI, INDIA",
                replyData.getFiller65().getReplyItem().get(2).getReplyPersonal().getReplyAddress());
        assertEquals("SIR MICHAEL ROBERTS",
                replyData.getFiller65().getReplyItem().get(2).getReplyPersonal().getReplyName());
        assertEquals("70331211", replyData.getFiller65().getReplyItem().get(2).getReplyPersonal().getReplyPhone());

        assertEquals(200000, replyData.getFiller65().getReplyItem().get(3).getReplyNumber());
        assertEquals("$0020.00", replyData.getFiller65().getReplyItem().get(3).getReplyAmount());
        assertEquals("*********", replyData.getFiller65().getReplyItem().get(3).getReplyComment());
        assertEquals("26 11 81", replyData.getFiller65().getReplyItem().get(3).getReplyDate());
        assertEquals("GLASGOW,  SCOTLAND",
                replyData.getFiller65().getReplyItem().get(3).getReplyPersonal().getReplyAddress());
        assertEquals("S. P. RUSSELL", replyData.getFiller65().getReplyItem().get(3).getReplyPersonal().getReplyName());
        assertEquals("63738290", replyData.getFiller65().getReplyItem().get(3).getReplyPersonal().getReplyPhone());
    }
}
