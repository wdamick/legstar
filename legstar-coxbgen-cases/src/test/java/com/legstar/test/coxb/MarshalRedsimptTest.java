package com.legstar.test.coxb;

import com.legstar.test.coxb.redsimpt.DfhcommareaType;

import junit.framework.TestCase;

public class MarshalRedsimptTest extends TestCase {

	private final static String SCHEMA_NAME = "redsimpt";
	
	public void testRedsimpt() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		dfhcommareaType.setCDefinition1("ABCDEFGHIJKLMNO");

		//		     <------------------------------------>
		//		      1 2 3 4 5 6 7 8 9 101112131415161718
		//		      A B C D E F G H I J K L M N O       
		assertEquals("c1c2c3c4c5c6c7c8c9d1d2d3d4d5d6404040",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 18));
	}

	public void testRedsimptSecondChoice() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		dfhcommareaType.setCDefinition2(123456789012345l);

		//		     <------------------------------------>
		//		      1 2 3 4 5 6 7 8 9 101112131415161718
		//		      0 0 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5        
		assertEquals("f0f0f0f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 18));
	}
}
