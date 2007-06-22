package com.legstar.test.coxb;

import com.legstar.test.coxb.numzoned.DfhcommareaType;

import junit.framework.TestCase;

public class MarshalNumzonedTest extends TestCase {

	private final static String SCHEMA_NAME = "numzoned";
	
	public void testLsfileae() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		dfhcommareaType.setLU(6);
		dfhcommareaType.setLS(Short.parseShort("-5"));
		dfhcommareaType.setLSSignL(Short.parseShort("-78"));
		dfhcommareaType.setLSSignT(Short.parseShort("1"));
		dfhcommareaType.setLSSignSL(Short.parseShort("9"));
		dfhcommareaType.setLSSignST(Short.parseShort("-11"));

		//		      <><--><----><--><--><---->
		//		      1 1 2 1 2 3 1 2 1 2 1 2 3  
		//		      6   -5 -7 8   +1 + 9 1 1 - 
		assertEquals("f6f0d5d0f7f8f0c14ef9f1f160",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 13));
	}
}
