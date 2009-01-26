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

import com.legstar.test.coxb.varar021.IStaticData;
import com.legstar.test.coxb.varar021.LkupInfo39;
import com.legstar.test.coxb.varar021.LkupInfo44;
import com.legstar.test.coxb.varar021.ODynamicData;
import com.legstar.test.coxb.varar021.ObjectFactory;
import com.legstar.test.coxb.varar021.Payload;
import com.legstar.test.coxb.varar021.SearchGrplst;
import com.legstar.test.coxb.varar021.WellpointEaiEbsErrorRow;

import junit.framework.TestCase;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class Varar021Cases extends TestCase {

    /** Utility class. */
    private Varar021Cases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static SearchGrplst getJavaObjectEmpty() {
        ObjectFactory of = new ObjectFactory();
        SearchGrplst searchGrplst = of.createSearchGrplst();
        Payload payload = new Payload();
        searchGrplst.setPayload(payload);
        return searchGrplst;
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexEmpty() { 

        return "f0f0f0"
        + "f0f0f0"
        + "f0f0f0"
        + "40";
    }

    /**
     * Check that data object contains the expected values.
     * @param searchGrplst the java object to check
     */
    public static void checkJavaObjectEmpty(final SearchGrplst searchGrplst) {
        assertEquals(0, searchGrplst.getPayload().getWechRequestRows());
        assertEquals(0, searchGrplst.getPayload().getWechDynamicResponseRows());
        assertEquals(0, searchGrplst.getPayload().getWechErrorRows());
        assertEquals("", searchGrplst.getPayload().getWechAdditionalPageKeys());
        assertTrue(null == searchGrplst.getPayload().getIStaticData());
        assertEquals(0, searchGrplst.getPayload().getODynamicData().size());
        assertEquals(0, searchGrplst.getPayload().getWellpointEaiEbsErrorRow().size());
    }
    /**
     * @return an instance of a valued java object.
     */
    public static SearchGrplst getJavaObjectNoIStaticData() {
        ObjectFactory of = new ObjectFactory();
        SearchGrplst searchGrplst = of.createSearchGrplst();
        Payload payload = new Payload();
        searchGrplst.setPayload(payload);
        /* No need to set counters they will automatically be updated */
        ODynamicData oDynamicData = of.createODynamicData();
        LkupInfo44 lkupInfo44n1 = of.createLkupInfo44();
        lkupInfo44n1.setLkupIdCt("ABCDEFGHIJKLMNOPQR");
        lkupInfo44n1.setLkupTypCdCt("12345");
        oDynamicData.getLkupInfo().add(lkupInfo44n1);
        LkupInfo44 lkupInfo44n2 = of.createLkupInfo44();
        lkupInfo44n2.setLkupIdCt("123456789012345678");
        lkupInfo44n2.setLkupTypCdCt("ABCDE");
        oDynamicData.getLkupInfo().add(lkupInfo44n2);
        payload.getODynamicData().add(oDynamicData);
        WellpointEaiEbsErrorRow wellpointEaiEbsErrorRow = of.createWellpointEaiEbsErrorRow();
        wellpointEaiEbsErrorRow.setWeerExceptionUuid("AN ERROR");
        payload.getWellpointEaiEbsErrorRow().add(wellpointEaiEbsErrorRow);
        return searchGrplst;
    }
 
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexNoIStaticData() { 

        return "f0f0f0"
        + "f0f0f1"
        + "f0f0f1"
        + "40"
        + "c1c2c3c4c5c6c7c8c9d1d2d3d4d5d6d7d8d9"
        + "f1f2f3f4f5"
        + "f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f6f7f8"
        + "c1c2c3c4c5"
        + "c1d540c5d9d9d6d9404040404040404040404040404040404040404040404040";
    }

    /**
     * Check that data object contains the expected values.
     * @param searchGrplst the java object to check
     */
    public static void checkJavaObjectNoIStaticData(final SearchGrplst searchGrplst) {
        assertEquals(0, searchGrplst.getPayload().getWechRequestRows());
        assertEquals(1, searchGrplst.getPayload().getWechDynamicResponseRows());
        assertEquals(1, searchGrplst.getPayload().getWechErrorRows());
        assertEquals("", searchGrplst.getPayload().getWechAdditionalPageKeys());
        assertTrue(null == searchGrplst.getPayload().getIStaticData());
        assertEquals(1, searchGrplst.getPayload().getODynamicData().size());
        assertEquals("ABCDEFGHIJKLMNOPQR",
                searchGrplst.getPayload().getODynamicData().get(0).getLkupInfo().get(0).getLkupIdCt());
        assertEquals("12345", searchGrplst.getPayload().getODynamicData().get(0).getLkupInfo().get(0).getLkupTypCdCt());
        assertEquals("123456789012345678",
                searchGrplst.getPayload().getODynamicData().get(0).getLkupInfo().get(1).getLkupIdCt());
        assertEquals("ABCDE", searchGrplst.getPayload().getODynamicData().get(0).getLkupInfo().get(1).getLkupTypCdCt());
        assertEquals(1, searchGrplst.getPayload().getWellpointEaiEbsErrorRow().size());
        assertEquals("AN ERROR", searchGrplst.getPayload().getWellpointEaiEbsErrorRow().get(0).getWeerExceptionUuid());
    }

    /**
     * @return an instance of a valued java object.
     */
    public static SearchGrplst getJavaObjectWithIStaticData() {
        ObjectFactory of = new ObjectFactory();
        SearchGrplst searchGrplst = getJavaObjectNoIStaticData();
        IStaticData iStaticData = of.createIStaticData();
        LkupInfo39 lkupInfo39n1 = of.createLkupInfo39();
        lkupInfo39n1.setLkupId("RQPONMLKJIHGFEDCBA");
        lkupInfo39n1.setLkupTypCd("54321");
        iStaticData.getLkupInfo().add(lkupInfo39n1);
        LkupInfo39 lkupInfo39n2 = of.createLkupInfo39();
        lkupInfo39n2.setLkupId("876543210987654321");
        lkupInfo39n2.setLkupTypCd("EDCBA");
        iStaticData.getLkupInfo().add(lkupInfo39n2);
        searchGrplst.getPayload().setIStaticData(iStaticData);
        searchGrplst.getPayload().setWechRequestRows(1);
        return searchGrplst;
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexWithIStaticData() { 

        return "f0f0f1"
        + "f0f0f1"
        + "f0f0f1"
        + "40"
        + "d9d8d7d6d5d4d3d2d1c9c8c7c6c5c4c3c2c1"
        + "f5f4f3f2f1"
        + "f8f7f6f5f4f3f2f1f0f9f8f7f6f5f4f3f2f1"
        + "c5c4c3c2c1"
        + "c1c2c3c4c5c6c7c8c9d1d2d3d4d5d6d7d8d9"
        + "f1f2f3f4f5"
        + "f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f6f7f8"
        + "c1c2c3c4c5"
        + "c1d540c5d9d9d6d9404040404040404040404040404040404040404040404040";
    }

    /**
     * Check that data object contains the expected values.
     * @param searchGrplst the java object to check
     */
    public static void checkJavaObjectWithIStaticData(final SearchGrplst searchGrplst) {
        assertEquals(1, searchGrplst.getPayload().getWechRequestRows());
        assertEquals(1, searchGrplst.getPayload().getWechDynamicResponseRows());
        assertEquals(1, searchGrplst.getPayload().getWechErrorRows());
        assertEquals("", searchGrplst.getPayload().getWechAdditionalPageKeys());
        assertEquals("RQPONMLKJIHGFEDCBA", searchGrplst.getPayload().getIStaticData().getLkupInfo().get(0).getLkupId());
        assertEquals("54321", searchGrplst.getPayload().getIStaticData().getLkupInfo().get(0).getLkupTypCd());
        assertEquals("876543210987654321", searchGrplst.getPayload().getIStaticData().getLkupInfo().get(1).getLkupId());
        assertEquals("EDCBA", searchGrplst.getPayload().getIStaticData().getLkupInfo().get(1).getLkupTypCd());
        assertEquals(1, searchGrplst.getPayload().getODynamicData().size());
        assertEquals("ABCDEFGHIJKLMNOPQR",
                searchGrplst.getPayload().getODynamicData().get(0).getLkupInfo().get(0).getLkupIdCt());
        assertEquals("12345", searchGrplst.getPayload().getODynamicData().get(0).getLkupInfo().get(0).getLkupTypCdCt());
        assertEquals("123456789012345678",
                searchGrplst.getPayload().getODynamicData().get(0).getLkupInfo().get(1).getLkupIdCt());
        assertEquals("ABCDE", searchGrplst.getPayload().getODynamicData().get(0).getLkupInfo().get(1).getLkupTypCdCt());
        assertEquals(1, searchGrplst.getPayload().getWellpointEaiEbsErrorRow().size());
        assertEquals("AN ERROR", searchGrplst.getPayload().getWellpointEaiEbsErrorRow().get(0).getWeerExceptionUuid());
    }

    /**
     * Check that data object contains an error report following a request that
     * contained an invalid I-STATIC-DATA structure.
     * @param searchGrplst the java object to check
     */
    public static void checkJavaObjectError(final SearchGrplst searchGrplst) {
        assertEquals(1, searchGrplst.getPayload().getWechRequestRows());
        assertEquals(0, searchGrplst.getPayload().getWechDynamicResponseRows());
        assertEquals(1, searchGrplst.getPayload().getWechErrorRows());
        assertEquals("", searchGrplst.getPayload().getWechAdditionalPageKeys());
        assertEquals("", searchGrplst.getPayload().getIStaticData().getLkupInfo().get(0).getLkupId());
        assertEquals("ALPHA", searchGrplst.getPayload().getIStaticData().getLkupInfo().get(0).getLkupTypCd());
        assertEquals("", searchGrplst.getPayload().getIStaticData().getLkupInfo().get(1).getLkupId());
        assertEquals("", searchGrplst.getPayload().getIStaticData().getLkupInfo().get(1).getLkupTypCd());
        assertEquals(0, searchGrplst.getPayload().getODynamicData().size());
        assertEquals(1, searchGrplst.getPayload().getWellpointEaiEbsErrorRow().size());
        assertEquals("LKUP-TYP-CD(1, 1) not numeric",
                searchGrplst.getPayload().getWellpointEaiEbsErrorRow().get(0).getWeerExceptionUuid());
    }

    /**
     * Check that data object contains 10 O-DYNAMIC-DATA data structures.
     * @param searchGrplst the java object to check
     */
    public static void checkJavaObject10Items(final SearchGrplst searchGrplst) {
        assertEquals(1, searchGrplst.getPayload().getWechRequestRows());
        assertEquals(10, searchGrplst.getPayload().getWechDynamicResponseRows());
        assertEquals(0, searchGrplst.getPayload().getWechErrorRows());
        assertEquals("", searchGrplst.getPayload().getWechAdditionalPageKeys());
        assertEquals("", searchGrplst.getPayload().getIStaticData().getLkupInfo().get(0).getLkupId());
        assertEquals("00010", searchGrplst.getPayload().getIStaticData().getLkupInfo().get(0).getLkupTypCd());
        assertEquals("", searchGrplst.getPayload().getIStaticData().getLkupInfo().get(1).getLkupId());
        assertEquals("", searchGrplst.getPayload().getIStaticData().getLkupInfo().get(1).getLkupTypCd());
        assertEquals(10, searchGrplst.getPayload().getODynamicData().size());
        for (int i = 0; i < searchGrplst.getPayload().getODynamicData().size(); i++) {
            LkupInfo44 lkupInfo44 = searchGrplst.getPayload().getODynamicData().get(i).getLkupInfo().get(0);
            assertEquals("", lkupInfo44.getLkupIdCt());
            assertEquals("00010", lkupInfo44.getLkupTypCdCt());
            
        }
        
    }
}
