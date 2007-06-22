package com.legstar.test.coxb;

import com.legstar.test.coxb.fixarsim.DfhcommareaType;

import junit.framework.TestCase;

public class MarshalFixarsimTest extends TestCase {

	private final static String SCHEMA_NAME = "fixarsim";
	
	public void testFixarsim() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		dfhcommareaType.getCArray().add("PREMI");
		dfhcommareaType.getCArray().add("DEUXI");
		dfhcommareaType.getCArray().add("TROIS");

		//		      <---------------------------->
		//		      1 2 3 4 5 6 7 8 9 101112131415
		//		      P R E M I D E U X I T R O I S             
		assertEquals("d7d9c5d4c9c4c5e4e7c9e3d9d6c9e2",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 15));
	}
}
