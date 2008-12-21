package com.legstar.test.coxb;

import junit.framework.TestCase;

import com.legstar.test.coxb.dplarcht.LsRequest;
import com.legstar.test.coxb.dplarcht.ObjectFactory;
import com.legstar.test.coxb.dplarcht.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class DplarchtCases extends TestCase {

    /** Utility class. */
    private DplarchtCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        LsRequest lsRequest = new LsRequest();
        lsRequest.setLsRequestType(0);
        lsRequest.setLsAllItems("*");

        dfhcommarea.setLsRequest(lsRequest);
        return dfhcommarea;
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final Dfhcommarea dfhcommarea) {
        
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return "0000"
        + "5c404040"
        + "4040404040404040"
        + "000000000f"
        + "0000"
        + "00000000"
        + "00000000000000000000"
        + "00000000000000000000"
        + "00000000000000000000"
        + "00000000000000000000"
        + "00000000000000000000"
        + "00000000000000000000000000";
    }
 
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex1Program() { 

        return "0001"
        + "5C404040"
        + "4040404040404040"
        + "000000000F"
        + "0000"
        + "00000001"
        + "C2C9D5C1D9C3C8E3"
        + "D7D9D6C7D9C1D44040404040"
        + "D5D6E3C4C5C6C9D5C5C44040"
        + "000016A0"
        + "00000002"
        + "000000000000000000000000000000000000000000000000"
        + "C2C9D5D5C1E3E2";
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject1Program(final Dfhcommarea dfhcommarea) {
        assertEquals(1, dfhcommarea.getLsReply().getLsReplyData().getLsItemsCount());
        assertEquals("NOTDEFINED", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsProgramsData().getLsProgramLanguage());
        assertEquals(5792, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsProgramsData().getLsProgramLength());
        assertEquals("BINARCHT", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsProgramsData().getLsProgramName());
        assertEquals("PROGRAM", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsProgramsData().getLsProgramType());
        assertEquals(2, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsProgramsData().getLsProgramUsecount());
        assertEquals("", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsProgramsData().getFiller113());
        assertEquals(null, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsFilesData());
        assertEquals(null, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsTransactionsData());
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex1Transaction() {
        return "0002"
        + "5c404040"
        + "4040404040404040"
        + "000000000f"
        + "0001"
        + "00000001"
        + "c1c1c1c1c1c1c1c1"
        + "c2c2c2c2c2c2c2c2"
        + "c3c3c3c3c3c3c3c3c3c3c3c3"
        + "404040404040404040404040404040404040404040404040404040404040404040404040";
    }
    
    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject1Transaction(final Dfhcommarea dfhcommarea) {
        assertEquals(1, dfhcommarea.getLsReply().getLsReplyData().getLsItemsCount());
        assertEquals("AAAAAAAA", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsTransactionsData().getLsTransactionName());
        assertEquals("BBBBBBBB", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsTransactionsData().getLsTransactionProgram());
        assertEquals("CCCCCCCCCCCC", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsTransactionsData().getLsTransactionStatus());
        assertEquals(null, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsFilesData());
        assertEquals(null, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsProgramsData());
    }
    
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex1File() {
        return "0000"
        + "5c404040"
        + "4040404040404040"
        + "000000000f"
        + "0000"
        + "00000001"
        + "c1c2c3c4c1c2c3c4"
        + "c4404040404040404040"
        + "c4404040404040404040"
        + "c4404040404040404040"
        + "c4404040404040404040"
        + "40404040"
        + "c1c1c1c1c2c2c2c2c3c3c3c3";
    }
    
    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject1File(final Dfhcommarea dfhcommarea) {
        assertEquals(1, dfhcommarea.getLsReply().getLsReplyData().getLsItemsCount());
        assertEquals("ABCDABCD", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsFilesData().getLsFileName());
        assertEquals("D         D         D         D", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsFilesData().getLsFileDsname());
        assertEquals("AAAABBBBCCCC", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsFilesData().getLsFileEnablestatus());
        assertEquals(null, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsProgramsData());
        assertEquals(null, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray()
                .get(0).getLsTransactionsData());
    }
}
