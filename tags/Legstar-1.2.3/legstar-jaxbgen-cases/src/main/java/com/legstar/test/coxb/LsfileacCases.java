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

import com.legstar.test.coxb.lsfileac.QueryData;
import com.legstar.test.coxb.lsfileac.QueryLimit;
import com.legstar.test.coxb.lsfileac.ReplyData;
import com.legstar.test.coxb.lsfileac.ReplyStatus;
import com.legstar.test.coxb.lsfileac.ObjectFactory;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class LsfileacCases extends TestCase {

    /** Utility class. */
    private LsfileacCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static QueryData getJavaObjectQueryData() {
        ObjectFactory of = new ObjectFactory();
        QueryData queryData = of.createQueryData();
        queryData.setQueryName("S*");
        queryData.setQueryAddress("*");
        queryData.setQueryPhone("*");
        return queryData;
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexQueryData() { 

        return
        /*  S *                                    */
        "e25c404040404040404040404040404040404040"
        /*  *                                      */
        + "5c40404040404040404040404040404040404040"
        /*  *               */
        + "5c40404040404040";
    }

    /**
     * @return an instance of a valued java object.
     */
    public static QueryLimit getJavaObjectQueryLimit() {
        ObjectFactory of = new ObjectFactory();
        QueryLimit queryLimit = of.createQueryLimit();
        queryLimit.setMaxElapseTime(5000); /* 5 seconds */
        queryLimit.setMaxItemsRead(100);
        return queryLimit;
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexQueryLimit() { 

        return
        /*  5000 *                                  */
        "000050000f"
        /*  100 *                                  */
        + "000001000f";
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexReplyData() { 

        return
        /*        5*/  
        "000000005f"
        + "f0f0f0f1f0f0"
        + "e24b40c44b40c2d6d9d4c1d54040404040404040"
        + "e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040"
        + "f3f2f1f5f6f7f7f8"
        + "f2f640f1f140f8f1"
        + "5bf0f1f0f04bf1f1"
        + "5c5c5c5c5c5c5c5c5c"

        + "f0f0f0f7f6f2"
        + "e2e4e2c1d540d4c1d3c1c9d2c140404040404040"
        + "e2c1d540d1d6e2c56bc3c1d3c9c6d6d9d5c9c140"
        + "f2f2f3f1f2f1f2f1"
        + "f0f140f0f640f7f4"
        + "5bf0f0f0f04bf0f0"
        + "5c5c5c5c5c5c5c5c5c"

        + "f0f0f6f0f1f6"
        + "e2c9d940d4c9c3c8c1c5d340d9d6c2c5d9e3e240"
        + "d5c5e640c4c5d3c8c96b40c9d5c4c9c140404040"
        + "f7f0f3f3f1f2f1f1"
        + "f2f140f0f540f7f4"
        + "5bf0f0f0f94bf8f8"
        + "5c5c5c5c5c5c5c5c5c"

        + "f2f0f0f0f0f0"
        + "e24b40d74b40d9e4e2e2c5d3d340404040404040"
        + "c7d3c1e2c7d6e66b4040e2c3d6e3d3c1d5c44040"
        + "f6f3f7f3f8f2f9f0"
        + "f2f640f1f140f8f1"
        + "5bf0f0f2f04bf0f0"
        + "5c5c5c5c5c5c5c5c5c"

        + "f5f5f5f5f5f5"
        + "e24bd14b40d3c1e9c5d5c2e84040404040404040"
        + "d2c9d5c7e2e3d6d56b40d54be84b404040404040"
        + "f3f9f9f4f4f4f2f0"
        + "f2f640f1f140f8f1"
        + "5bf0f0f0f54bf0f0"
        + "5c5c5c5c5c5c5c5c5c";
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexReplyStatus() { 

        return
        "0000"
        + "f0f07af0f07af0f0"
        + "000000044f"
        + "00000000"
        + "00000000"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040";
    }

    /** 
     * Check the values returned from LSFILEAC after they were transformed to Java.
     * @param replyStatus the java data object
     */
    public static void checkJavaObjectReplyStatus(final ReplyStatus replyStatus) {
        assertEquals(0, replyStatus.getReplyType());
        assertEquals(44, replyStatus.getTotalItemsRead());
        assertEquals("", replyStatus.getReplyMessage().trim());
        assertEquals(0, replyStatus.getReplyResp());
        assertEquals(0, replyStatus.getReplyResp2());
    }

    /** 
     * Check the values returned from LSFILEAC after they were transformed to Java.
     * Filtered with "S*"
     * @param replyData the java data object
     */
    public static void checkJavaObjectReplyData(final ReplyData replyData) {
        assertEquals("S. D. BORMAN",
                replyData.getReplyItem().get(0).getReplyPersonal().getReplyName());
        assertEquals("SUSAN MALAIKA",
                replyData.getReplyItem().get(1).getReplyPersonal().getReplyName());
        assertEquals("SIR MICHAEL ROBERTS",
                replyData.getReplyItem().get(2).getReplyPersonal().getReplyName());
        assertEquals("S. P. RUSSELL",
                replyData.getReplyItem().get(3).getReplyPersonal().getReplyName());
        assertEquals("S.J. LAZENBY",
                replyData.getReplyItem().get(4).getReplyPersonal().getReplyName());
    }

    /** 
     * Check the values returned from LSFILEAC after they were transformed to Java.
     * No filter.
     * @param replyData the java data object
     */
    public static void checkJavaObjectReplyDataFull(final ReplyData replyData) {
        assertEquals("S. D. BORMAN",
                replyData.getReplyItem().get(0).getReplyPersonal().getReplyName());
        assertEquals("J. T. CZAYKOWSKI",
                replyData.getReplyItem().get(1).getReplyPersonal().getReplyName());
        assertEquals("M. B. DOMBEY",
                replyData.getReplyItem().get(2).getReplyPersonal().getReplyName());
        assertEquals("A. I. HICKSON",
                replyData.getReplyItem().get(3).getReplyPersonal().getReplyName());
        assertEquals("ALAN TULIP",
                replyData.getReplyItem().get(4).getReplyPersonal().getReplyName());
    }

    /**
     * @return an instance of a valued java object.
     */
    public static QueryData getJavaObjectQueryDataNoMatch() {
        ObjectFactory of = new ObjectFactory();
        QueryData queryData = of.createQueryData();
        queryData.setQueryName("Z*");
        queryData.setQueryAddress("*");
        queryData.setQueryPhone("*");
        return queryData;
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexQueryDataNoMatch() { 

        return
        /*  Z *                                    */
        "e95c404040404040404040404040404040404040"
        /*  *                                      */
        + "5c40404040404040404040404040404040404040"
        /*  *               */
        + "5c40404040404040";
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexReplyStatusNoMatch() { 

        return
        "0000"
        + "f0f07af0f07af0f0"
        + "000000044f"
        + "00000000"
        + "00000000"
        + "d5d640c3e4e2e3d6d4c5d940e2c1e3c9e2c6c9c5e240e8d6e4d940d8e4c5d9e8"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040";
    }

    /** 
     * Check the values returned from LSFILEAC after they were transformed to Java.
     * @param replyStatus the java data object
     */
    public static void checkJavaObjectReplyStatusNoMatch(final ReplyStatus replyStatus) {
        assertEquals(0, replyStatus.getReplyType());
        assertEquals(44, replyStatus.getTotalItemsRead());
        assertEquals("NO CUSTOMER SATISFIES YOUR QUERY", replyStatus.getReplyMessage().trim());
        assertEquals(0, replyStatus.getReplyResp());
        assertEquals(0, replyStatus.getReplyResp2());
    }

}
