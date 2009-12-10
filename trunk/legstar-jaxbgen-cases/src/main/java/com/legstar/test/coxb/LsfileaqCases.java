package com.legstar.test.coxb;

import com.legstar.test.coxb.lsfileaq.Customer;
import com.legstar.test.coxb.lsfileaq.Dfhcommarea;
import com.legstar.test.coxb.lsfileaq.PersonalData;
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
	 * @return a hexadecimal representation of host data.
	 */
	public static String getHostBytesHexRequest5() { 

		return "e25c404040404040404040404040404040404040"
		+ "0005"
		+ "000000000f";
	}

	/**
	 * @return an instance of a java object with default values.
	 */
	public static Dfhcommarea getJavaObjectRequestDefaults() {
		Dfhcommarea dfhcommarea = new Dfhcommarea();
		QueryData queryData = new QueryData();
		dfhcommarea.setQueryData(queryData);
		return dfhcommarea;
	}

	/**
	 * @return a hexadecimal representation of host data.
	 */
	public static String getHostBytesHexRequestDefaults() { 

		return "4040404040404040404040404040404040404040"
		+ "ffff"
		+ "000000000f";
	}

	/**
	 * @return an instance of a valued java object.
	 */
	public static Dfhcommarea getJavaObjectRequestReply5() {
		Dfhcommarea dfhcommarea = new Dfhcommarea();
		QueryData queryData = new QueryData();
		queryData.setCustomerName("S*");
		queryData.setMaxReplies((short) 5);
		dfhcommarea.setQueryData(queryData);
		ReplyData replyData = new ReplyData();
		replyData.setReplyCount(5);
		replyData.getCustomer().add(createCustomer(100, "S. D. BORMAN", "SURREY, ENGLAND", "32156778", "26 11 81", "$0100.11", "*********"));
		replyData.getCustomer().add(createCustomer(762, "SUSAN MALAIKA", "SAN JOSE,CALIFORNIA", "22312121", "01 06 74", "$0000.00", "*********"));
		replyData.getCustomer().add(createCustomer(6016, "SIR MICHAEL ROBERTS", "NEW DELHI, INDIA", "70331211", "21 05 74", "$0009.88", "*********"));
		replyData.getCustomer().add(createCustomer(200000, "S. P. RUSSELL", "GLASGOW,  SCOTLAND", "63738290", "26 11 81", "$0020.00", "*********"));
		replyData.getCustomer().add(createCustomer(555555, "S.J. LAZENBY", "KINGSTON, N.Y.", "39944420", "26 11 81", "$0005.00", "*********"));
		dfhcommarea.setReplyData(replyData);
		return dfhcommarea;
	}

	/**
	 * @return a hexadecimal representation of host data.
	 */
	public static String getHostBytesHexRequestReply5() { 

		return ""
		+ "e25c404040404040404040404040404040404040"
		+ "0005"
		+ "000000005f"
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
	 * Helper to create a single customer.
	 * @param customerId customer ID
	 * @param customerName name
	 * @param customerAddress address
	 * @param customerPhone phone
	 * @param lastTransDate last transaction date
	 * @param lastTransAmount last transaction amount
	 * @param lastTransComment last comment
	 * @return
	 */
	protected static Customer createCustomer(
			final long customerId,
			final String customerName,
			final String customerAddress,
			final String customerPhone,
			final String lastTransDate,
			final String lastTransAmount,
			final String lastTransComment) {
		Customer customer = new Customer();
		customer.setCustomerId(customerId);
		PersonalData personalData = new PersonalData();
		personalData.setCustomerName(customerName);
		personalData.setCustomerAddress(customerAddress);
		personalData.setCustomerPhone(customerPhone);
		customer.setPersonalData(personalData);
		customer.setLastTransDate(lastTransDate);
		customer.setLastTransAmount(lastTransAmount);
		customer.setLastTransComment(lastTransComment);
		return customer;

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

		assertEquals(555555, replyData.getCustomer().get(4).getCustomerId());
		assertEquals("$0005.00", replyData.getCustomer().get(4).getLastTransAmount());
		assertEquals("*********", replyData.getCustomer().get(4).getLastTransComment());
		assertEquals("26 11 81", replyData.getCustomer().get(4).getLastTransDate());
		assertEquals("KINGSTON, N.Y.",
				replyData.getCustomer().get(4).getPersonalData().getCustomerAddress());
		assertEquals("S.J. LAZENBY", replyData.getCustomer().get(4).getPersonalData().getCustomerName());
		assertEquals("39944420", replyData.getCustomer().get(4).getPersonalData().getCustomerPhone());
	}

	/** 
	 * Check the values returned from LSFILAQ but using second date alternative.
	 * @param dfhcommarea the java data object
	 */
	public static void checkJavaObjectReplyAlt5(final Dfhcommarea reply) {
		ReplyData replyData = reply.getReplyData();
		assertNotNull(replyData);
		assertEquals(5, replyData.getReplyCount());
		assertEquals(100, replyData.getCustomer().get(0).getCustomerId());
		assertEquals("$0100.11", replyData.getCustomer().get(0).getLastTransAmount());
		assertEquals("*********", replyData.getCustomer().get(0).getLastTransComment());
		assertEquals("26", replyData.getCustomer().get(0).getFiller49().getLastTransDay());
		assertEquals("11", replyData.getCustomer().get(0).getFiller49().getLastTransMonth());
		assertEquals("81", replyData.getCustomer().get(0).getFiller49().getLastTransYear());
		assertEquals("SURREY, ENGLAND",
				replyData.getCustomer().get(0).getPersonalData().getCustomerAddress());
		assertEquals("S. D. BORMAN", replyData.getCustomer().get(0).getPersonalData().getCustomerName());
		assertEquals("32156778", replyData.getCustomer().get(0).getPersonalData().getCustomerPhone());

		assertEquals(762, replyData.getCustomer().get(1).getCustomerId());
		assertEquals("$0000.00", replyData.getCustomer().get(1).getLastTransAmount());
		assertEquals("*********", replyData.getCustomer().get(1).getLastTransComment());
		assertEquals("01", replyData.getCustomer().get(1).getFiller49().getLastTransDay());
		assertEquals("06", replyData.getCustomer().get(1).getFiller49().getLastTransMonth());
		assertEquals("74", replyData.getCustomer().get(1).getFiller49().getLastTransYear());
		assertEquals("SAN JOSE,CALIFORNIA",
				replyData.getCustomer().get(1).getPersonalData().getCustomerAddress());
		assertEquals("SUSAN MALAIKA", replyData.getCustomer().get(1).getPersonalData().getCustomerName());
		assertEquals("22312121", replyData.getCustomer().get(1).getPersonalData().getCustomerPhone());

		assertEquals(6016, replyData.getCustomer().get(2).getCustomerId());
		assertEquals("$0009.88", replyData.getCustomer().get(2).getLastTransAmount());
		assertEquals("*********", replyData.getCustomer().get(2).getLastTransComment());
		assertEquals("21", replyData.getCustomer().get(2).getFiller49().getLastTransDay());
		assertEquals("05", replyData.getCustomer().get(2).getFiller49().getLastTransMonth());
		assertEquals("74", replyData.getCustomer().get(2).getFiller49().getLastTransYear());
		assertEquals("NEW DELHI, INDIA",
				replyData.getCustomer().get(2).getPersonalData().getCustomerAddress());
		assertEquals("SIR MICHAEL ROBERTS",
				replyData.getCustomer().get(2).getPersonalData().getCustomerName());
		assertEquals("70331211", replyData.getCustomer().get(2).getPersonalData().getCustomerPhone());

		assertEquals(200000, replyData.getCustomer().get(3).getCustomerId());
		assertEquals("$0020.00", replyData.getCustomer().get(3).getLastTransAmount());
		assertEquals("*********", replyData.getCustomer().get(3).getLastTransComment());
		assertEquals("26", replyData.getCustomer().get(3).getFiller49().getLastTransDay());
		assertEquals("11", replyData.getCustomer().get(3).getFiller49().getLastTransMonth());
		assertEquals("81", replyData.getCustomer().get(3).getFiller49().getLastTransYear());
		assertEquals("GLASGOW,  SCOTLAND",
				replyData.getCustomer().get(3).getPersonalData().getCustomerAddress());
		assertEquals("S. P. RUSSELL", replyData.getCustomer().get(3).getPersonalData().getCustomerName());
		assertEquals("63738290", replyData.getCustomer().get(3).getPersonalData().getCustomerPhone());

		assertEquals(555555, replyData.getCustomer().get(4).getCustomerId());
		assertEquals("$0005.00", replyData.getCustomer().get(4).getLastTransAmount());
		assertEquals("*********", replyData.getCustomer().get(4).getLastTransComment());
		assertEquals("26", replyData.getCustomer().get(4).getFiller49().getLastTransDay());
		assertEquals("11", replyData.getCustomer().get(4).getFiller49().getLastTransMonth());
		assertEquals("81", replyData.getCustomer().get(4).getFiller49().getLastTransYear());
		assertEquals("KINGSTON, N.Y.",
				replyData.getCustomer().get(4).getPersonalData().getCustomerAddress());
		assertEquals("S.J. LAZENBY", replyData.getCustomer().get(4).getPersonalData().getCustomerName());
		assertEquals("39944420", replyData.getCustomer().get(4).getPersonalData().getCustomerPhone());
	}

	/**
	 * @return an XML representation of the request/reply for a selection of
	 * customers whose name starts with S.
	 */
	public static String getXmlRequestReply5() {
		return
		"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
		+ "<Dfhcommarea xmlns=\"http://legstar.com/test/coxb/lsfileaq\">"
		+ "<QueryData>"
		+ "<CustomerName>S*</CustomerName>"
		+ "<MaxReplies>5</MaxReplies>"
		+ "</QueryData>"
		+ "<ReplyData>"
		+ "<ReplyCount>5</ReplyCount>"
		+ "<Customer>"
		+ "<CustomerId>100</CustomerId>"
		+ "<PersonalData>"
		+ "<CustomerName>S. D. BORMAN</CustomerName>"
		+ "<CustomerAddress>SURREY, ENGLAND</CustomerAddress>"
		+ "<CustomerPhone>32156778</CustomerPhone>"
		+ "</PersonalData>"
		+ "<LastTransDate>26 11 81</LastTransDate>"
		+ "<LastTransAmount>$0100.11</LastTransAmount>"
		+ "<LastTransComment>*********</LastTransComment>"
		+ "</Customer>"
		+ "<Customer>"
		+ "<CustomerId>762</CustomerId>"
		+ "<PersonalData>"
		+ "<CustomerName>SUSAN MALAIKA</CustomerName>"
		+ "<CustomerAddress>SAN JOSE,CALIFORNIA</CustomerAddress>"
		+ "<CustomerPhone>22312121</CustomerPhone>"
		+ "</PersonalData>"
		+ "<LastTransDate>01 06 74</LastTransDate>"
		+ "<LastTransAmount>$0000.00</LastTransAmount>"
		+ "<LastTransComment>*********</LastTransComment>"
		+ "</Customer>"
		+ "<Customer>"
		+ "<CustomerId>6016</CustomerId>"
		+ "<PersonalData>"
		+ "<CustomerName>SIR MICHAEL ROBERTS</CustomerName>"
		+ "<CustomerAddress>NEW DELHI, INDIA</CustomerAddress>"
		+ "<CustomerPhone>70331211</CustomerPhone>"
		+ "</PersonalData>"
		+ "<LastTransDate>21 05 74</LastTransDate>"
		+ "<LastTransAmount>$0009.88</LastTransAmount>"
		+ "<LastTransComment>*********</LastTransComment>"
		+ "</Customer>"
		+ "<Customer>"
		+ "<CustomerId>200000</CustomerId>"
		+ "<PersonalData><CustomerName>S. P. RUSSELL</CustomerName>"
		+ "<CustomerAddress>GLASGOW,  SCOTLAND</CustomerAddress>"
		+ "<CustomerPhone>63738290</CustomerPhone>"
		+ "</PersonalData>"
		+ "<LastTransDate>26 11 81</LastTransDate>"
		+ "<LastTransAmount>$0020.00</LastTransAmount>"
		+ "<LastTransComment>*********</LastTransComment>"
		+ "</Customer>"
		+ "<Customer>"
		+ "<CustomerId>555555</CustomerId>"
		+ "<PersonalData>"
		+ "<CustomerName>S.J. LAZENBY</CustomerName>"
		+ "<CustomerAddress>KINGSTON, N.Y.</CustomerAddress>"
		+ "<CustomerPhone>39944420</CustomerPhone>"
		+ "</PersonalData>"
		+ "<LastTransDate>26 11 81</LastTransDate>"
		+ "<LastTransAmount>$0005.00</LastTransAmount>"
		+ "<LastTransComment>*********</LastTransComment>"
		+ "</Customer>"
		+ "</ReplyData>"
		+ "</Dfhcommarea>";

	}


}
