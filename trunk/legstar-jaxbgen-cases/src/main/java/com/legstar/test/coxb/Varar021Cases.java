package com.legstar.test.coxb;

import com.legstar.test.coxb.varar021.IStaticData;
import com.legstar.test.coxb.varar021.LkupInfo36;
import com.legstar.test.coxb.varar021.LkupInfo41;
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
        LkupInfo41 lkupInfo41_1 = of.createLkupInfo41();
        lkupInfo41_1.setLkupIdCt("ABCDEFGHIJKLMNOPQR");
        lkupInfo41_1.setLkupTypCdCt("12345");
        oDynamicData.getLkupInfo().add(lkupInfo41_1);
        LkupInfo41 lkupInfo41_2 = of.createLkupInfo41();
        lkupInfo41_2.setLkupIdCt("123456789012345678");
        lkupInfo41_2.setLkupTypCdCt("ABCDE");
        oDynamicData.getLkupInfo().add(lkupInfo41_2);
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
        assertEquals("ABCDEFGHIJKLMNOPQR", searchGrplst.getPayload().getODynamicData().get(0).getLkupInfo().get(0).getLkupIdCt());
        assertEquals("12345", searchGrplst.getPayload().getODynamicData().get(0).getLkupInfo().get(0).getLkupTypCdCt());
        assertEquals("123456789012345678", searchGrplst.getPayload().getODynamicData().get(0).getLkupInfo().get(1).getLkupIdCt());
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
        LkupInfo36 lkupInfo36_1 = of.createLkupInfo36();
        lkupInfo36_1.setLkupId("RQPONMLKJIHGFEDCBA");
        lkupInfo36_1.setLkupTypCd("54321");
        iStaticData.getLkupInfo().add(lkupInfo36_1);
        LkupInfo36 lkupInfo36_2 = of.createLkupInfo36();
        lkupInfo36_2.setLkupId("876543210987654321");
        lkupInfo36_2.setLkupTypCd("EDCBA");
        iStaticData.getLkupInfo().add(lkupInfo36_2);
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
        assertEquals("ABCDEFGHIJKLMNOPQR", searchGrplst.getPayload().getODynamicData().get(0).getLkupInfo().get(0).getLkupIdCt());
        assertEquals("12345", searchGrplst.getPayload().getODynamicData().get(0).getLkupInfo().get(0).getLkupTypCdCt());
        assertEquals("123456789012345678", searchGrplst.getPayload().getODynamicData().get(0).getLkupInfo().get(1).getLkupIdCt());
        assertEquals("ABCDE", searchGrplst.getPayload().getODynamicData().get(0).getLkupInfo().get(1).getLkupTypCdCt());
        assertEquals(1, searchGrplst.getPayload().getWellpointEaiEbsErrorRow().size());
        assertEquals("AN ERROR", searchGrplst.getPayload().getWellpointEaiEbsErrorRow().get(0).getWeerExceptionUuid());
    }

}
