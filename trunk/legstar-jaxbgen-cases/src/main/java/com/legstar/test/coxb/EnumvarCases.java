package com.legstar.test.coxb;

import java.math.BigInteger;

import com.legstar.test.coxb.enumvar.ObjectFactory;
import com.legstar.test.coxb.enumvar.SafeSearchOptionsType;
import com.legstar.test.coxb.enumvar.SearchRequestType;

import junit.framework.TestCase;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public class EnumvarCases extends TestCase {

    /** Constant LIVE ID.*/
    private static final String LIVE_ID = "5588C3ACE949315B3ECAADDA908611BDF5D8D5AA";

    /** Utility class. */
    private EnumvarCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static SearchRequestType getJavaObject() {
        ObjectFactory of = new ObjectFactory();
        SearchRequestType searchRequest = of.createSearchRequestType();
        searchRequest.setAppID(LIVE_ID);
        searchRequest.setSafeSearch(SafeSearchOptionsType.OFF);
        searchRequest.setSearchWeight(new BigInteger("4"));
        return searchRequest;
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() {
        /*       5 5 8 8 C 3 A C E 9 4 9 3 1 5 B 3 E C A A D D A 9 0 8 6 1 1 B D F 5 D 8 D 5 A A O f f*/
        return "f5f5f8f8c3f3c1c3c5f9f4f9f3f1f5c2f3c5c3c1c1c4c4c1f9f0f8f6f1f1c2c4c6f5c4f8c4f5c1c1d68686"
        /*                                                              4*/
        + "40404040404040404040404040404040404040404040404040404040400004";
    }

    /**
     * Check that data object contains the expected values.
     * @param dfhcommarea the java object to check
     */
    public static void checkJavaObject(final SearchRequestType searchRequest) {
        assertEquals(LIVE_ID, searchRequest.getAppID());
        assertEquals(SafeSearchOptionsType.OFF, searchRequest.getSafeSearch());
        assertEquals("4", searchRequest.getSearchWeight().toString());
    }
    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }
}
