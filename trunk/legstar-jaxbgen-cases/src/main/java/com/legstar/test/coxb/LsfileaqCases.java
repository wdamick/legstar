package com.legstar.test.coxb;

import com.legstar.test.coxb.lsfileaq.Dfhcommarea;
import com.legstar.test.coxb.lsfileaq.QueryData;
import com.legstar.test.coxb.lsfileaq.ReplyData;

import junit.framework.TestCase;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public class LsfileaqCases extends TestCase {

    /**
     * @return an instance of a valued java object.
     */
	public static Dfhcommarea getJavaObjectRequest5() {
    	Dfhcommarea dfhcommarea = new Dfhcommarea();
    	QueryData queryData = new QueryData();
    	queryData.setCustomerName("S*");
    	queryData.setMaxReplies((short) 5);
    	dfhcommarea.setQueryData(queryData);
    	return dfhcommarea;
    }
    
    /** 
     * Check the values returned from LSFILAQ after they were transformed to Java.
     * @param dfhcommarea the java data object
     */
    public static void checkJavaObjectReply5(final Dfhcommarea reply) {
    	ReplyData replyData = reply.getReplyData();
    	assertNotNull(replyData);
    	assertEquals(5, replyData.getReplyCount());
        assertEquals(100, replyData.getCustomer().get(0).getCustomerId());
        assertEquals("$0100.11", replyData.getCustomer().get(0).getLastTransAmount());
        assertEquals("*********", replyData.getCustomer().get(0).getLastTransComment());
        assertEquals("26 11 81", replyData.getCustomer().get(0).getLastTransDate());
        assertEquals("SURREY, ENGLAND",
                replyData.getCustomer().get(0).getPersonalData().getCustomerAddress());
        assertEquals("S. D. BORMAN", replyData.getCustomer().get(0).getPersonalData().getCustomerName());
        assertEquals("32156778", replyData.getCustomer().get(0).getPersonalData().getCustomerPhone());

        assertEquals(762, replyData.getCustomer().get(1).getCustomerId());
        assertEquals("$0000.00", replyData.getCustomer().get(1).getLastTransAmount());
        assertEquals("*********", replyData.getCustomer().get(1).getLastTransComment());
        assertEquals("01 06 74", replyData.getCustomer().get(1).getLastTransDate());
        assertEquals("SAN JOSE,CALIFORNIA",
                replyData.getCustomer().get(1).getPersonalData().getCustomerAddress());
        assertEquals("SUSAN MALAIKA", replyData.getCustomer().get(1).getPersonalData().getCustomerName());
        assertEquals("22312121", replyData.getCustomer().get(1).getPersonalData().getCustomerPhone());

        assertEquals(6016, replyData.getCustomer().get(2).getCustomerId());
        assertEquals("$0009.88", replyData.getCustomer().get(2).getLastTransAmount());
        assertEquals("*********", replyData.getCustomer().get(2).getLastTransComment());
        assertEquals("21 05 74", replyData.getCustomer().get(2).getLastTransDate());
        assertEquals("NEW DELHI, INDIA",
                replyData.getCustomer().get(2).getPersonalData().getCustomerAddress());
        assertEquals("SIR MICHAEL ROBERTS",
                replyData.getCustomer().get(2).getPersonalData().getCustomerName());
        assertEquals("70331211", replyData.getCustomer().get(2).getPersonalData().getCustomerPhone());

        assertEquals(200000, replyData.getCustomer().get(3).getCustomerId());
        assertEquals("$0020.00", replyData.getCustomer().get(3).getLastTransAmount());
        assertEquals("*********", replyData.getCustomer().get(3).getLastTransComment());
        assertEquals("26 11 81", replyData.getCustomer().get(3).getLastTransDate());
        assertEquals("GLASGOW,  SCOTLAND",
                replyData.getCustomer().get(3).getPersonalData().getCustomerAddress());
        assertEquals("S. P. RUSSELL", replyData.getCustomer().get(3).getPersonalData().getCustomerName());
        assertEquals("63738290", replyData.getCustomer().get(3).getPersonalData().getCustomerPhone());
    }


}
