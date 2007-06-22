package com.legstar.test.coxb;

import com.legstar.test.coxb.lsfilead.DfhcommareaType;

import junit.framework.TestCase;

public class MarshalLsfileadTest extends TestCase {

	private final static String SCHEMA_NAME = "lsfilead";
	
	public void testLsfilead() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		dfhcommareaType.setComNumber(100l);
		dfhcommareaType.setComName("TOTO");
		dfhcommareaType.setComAddress("LABAS STREET");
		dfhcommareaType.setComPhone("88993314");
		dfhcommareaType.setComDate("100458");
		dfhcommareaType.setComAmount("00100.35");
		dfhcommareaType.setComComment("A VOIR");

		//		              <----------><--------------------------------------><--------------------------------------><--------------><--------------><--------------><---------------->
		//		              1 2 3 4 5 6 1 2 3 4 5 6 7 8 9 10111213141516171819201 2 3 4 5 6 7 8 9 10111213141516171819201 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 9
		//		              0 0 0 1 0 0 T O T O                                 L A B A S   S T R E E T                 8 8 9 9 3 3 1 4 1 0 0 4 5 8 0 0 1 0 0 . 3 5 A   V O I R
		assertEquals("f0f0f0f1f0f0e3d6e3d640404040404040404040404040404040d3c1c2c1e240e2e3d9c5c5e34040404040404040f8f8f9f9f3f3f1f4f1f0f0f4f5f84040f0f0f1f0f04bf3f5c140e5d6c9d9404040",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 79));
	}
}
