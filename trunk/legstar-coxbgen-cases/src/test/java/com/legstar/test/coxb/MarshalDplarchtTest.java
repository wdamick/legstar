package com.legstar.test.coxb;

import com.legstar.test.coxb.dplarcht.DfhcommareaType;
import com.legstar.test.coxb.dplarcht.LsRequestType;

import junit.framework.TestCase;

public class MarshalDplarchtTest extends TestCase {

	private final static String SCHEMA_NAME = "dplarcht";

	public void testDplarcht() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		LsRequestType lsRequestType = new LsRequestType();
		lsRequestType.setLsRequestType(0);
		lsRequestType.setLsAllItems("*");

		dfhcommareaType.setLsRequest(lsRequestType);

		assertEquals("00005c4040404040404040404040000000000f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 88));
	}
}
