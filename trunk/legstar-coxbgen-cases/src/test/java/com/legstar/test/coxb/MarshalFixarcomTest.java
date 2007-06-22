package com.legstar.test.coxb;

import com.legstar.test.coxb.fixarcom.CArrayType;
import com.legstar.test.coxb.fixarcom.DfhcommareaType;

import junit.framework.TestCase;

public class MarshalFixarcomTest extends TestCase {

	private final static String SCHEMA_NAME = "fixarcom";
	
	public void testFixarcom() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);

		for (int i = 0; i < 7; i++) {
			CArrayType cArrayType = new CArrayType();
			cArrayType.setCItem1("ABJA" + Integer.toString(i));
			cArrayType.setCItem2(Short.parseShort(Integer.toString(7 * i)));
			dfhcommareaType.getCArray().add(cArrayType);
		}
		
		//		      <------------><------------><------------><------------><------------><------------><------------>
		//		      1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 
		//		      A B J A 0    0A B J A 1    7A B J A 0   14A B J A 3   21A B J A 4   28A B J A 5   35A B J A 6   42             
		assertEquals("c1c2d1c1f00000c1c2d1c1f10007c1c2d1c1f2000ec1c2d1c1f30015c1c2d1c1f4001cc1c2d1c1f50023c1c2d1c1f6002a",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 49));
	}
}
