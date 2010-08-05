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

import com.legstar.test.coxb.dplarcht.LsFilesData;
import com.legstar.test.coxb.dplarcht.LsItemsArray;
import com.legstar.test.coxb.dplarcht.LsProgramsData;
import com.legstar.test.coxb.dplarcht.LsReply;
import com.legstar.test.coxb.dplarcht.LsReplyData;
import com.legstar.test.coxb.dplarcht.LsRequest;
import com.legstar.test.coxb.dplarcht.ObjectFactory;
import com.legstar.test.coxb.dplarcht.Dfhcommarea;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public class DplarchtCases extends TestCase {

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
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObject1Program() {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        LsRequest lsRequest = new LsRequest();
        lsRequest.setLsRequestType(1);
        lsRequest.setLsAllItems("*");
        LsReply lsReply = of.createLsReply();
        LsReplyData lsReplyData = of.createLsReplyData();
        LsProgramsData lsProgramsData = of.createLsProgramsData();
        lsProgramsData.setLsProgramLanguage("NOTDEFINED");
        lsProgramsData.setLsProgramLength(5792);
        lsProgramsData.setLsProgramName("BINARCHT");
        lsProgramsData.setLsProgramType("PROGRAM");
        lsProgramsData.setLsProgramUsecount(2);
        LsItemsArray lsItemsArray = of.createLsItemsArray();
        lsItemsArray.setLsProgramsData(lsProgramsData);
        lsReplyData.setLsItemsCount(1);
        lsReplyData.getLsItemsArray().add(lsItemsArray);
        
        lsReply.setLsReplyType(0);
        lsReply.setLsReplyData(lsReplyData);

        dfhcommarea.setLsRequest(lsRequest);
        dfhcommarea.setLsReply(lsReply);
        return dfhcommarea;
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex1Program() { 

        return "0001"
        + "5c404040"
        + "4040404040404040"
        + "000000000f"
        + "0000"
        + "00000001"
        + "c2c9d5c1d9c3c8e3"
        + "d7d9d6c7d9c1d44040404040"
        + "d5d6e3c4c5c6c9d5c5c44040"
        + "000016a0"
        + "00000002"
        + "404040404040404040404040404040404040404040404040";
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
    
    /**
     * Simulates a host buffer with the requested number of files.
     * @param files how many files to simulate
     * @return a hexadecimal representation of host data
     */
    public static String getHostBytesHexFiles(final int files) {
        StringBuilder sb = new StringBuilder();
        String strFiles = Integer.toHexString(files);
        sb.append("0000"
                + "5c404040"
                + "4040404040404040"
                + "000000000f"
                + "0000"
                + "00000000".substring(0, 8 - strFiles.length()) + strFiles);
        for (int i = 0; i < files; i++) {
            sb.append("c1c2c3c4c1c2c3c4"
                    + "c4404040404040404040"
                    + "c4404040404040404040"
                    + "c4404040404040404040"
                    + "c4404040404040404040"
                    + "40404040"
                    + "c1c1c1c1c2c2c2c2c3c3c3c3");
        }
        return sb.toString();
    }

    /**
     * Simulates a jav object with the requested number of files.
     * @param files number of files to simulate  
     * @return an instance of a valued java object.
     */
    public static Dfhcommarea getJavaObjectFiles(final int files) {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        LsRequest lsRequest = of.createLsRequest();
        dfhcommarea.setLsRequest(lsRequest);
        lsRequest.setLsRequestType(0);
        lsRequest.setLsAllItems("*");
        com.legstar.test.coxb.dplarcht.LsSearchCriteria lsSearchCriteria = of.createLsSearchCriteria();
        lsSearchCriteria.setLsStartwith("");
        lsSearchCriteria.setLsStartwithLen(0);
        lsRequest.setLsSearchCriteria(lsSearchCriteria);
        
        com.legstar.test.coxb.dplarcht.LsReply lsReply = of.createLsReply();
        dfhcommarea.setLsReply(lsReply);
        com.legstar.test.coxb.dplarcht.LsReplyData lsReplyData = of.createLsReplyData();
        lsReply.setLsReplyData(lsReplyData);
        lsReplyData.setLsItemsCount(files);
        
        for (int i = 0; i < files; i++) {
            LsItemsArray ia = of.createLsItemsArray();
            LsFilesData dt = of.createLsFilesData();
            dt.setLsFileName("ABCDABCD");
            dt.setLsFileEnablestatus("AAAABBBBCCCC");
            ia.setLsFilesData(dt);
            lsReplyData.getLsItemsArray().add(ia);
        }
        return dfhcommarea;
    }
    
    /**
     * Check that data object contains the expected values.
     * This is used for volume testing where we avoid to compare the entire payload
     * which would impact CPU.
     * @param files number of files to simulate  
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObjectFiles(final int files, final Dfhcommarea dfhcommarea) {
        assertEquals(files, dfhcommarea.getLsReply().getLsReplyData().getLsItemsCount());
    }
    
    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }
}
