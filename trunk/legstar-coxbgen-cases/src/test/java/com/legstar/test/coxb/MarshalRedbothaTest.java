package com.legstar.test.coxb;

import com.legstar.test.coxb.redbotha.Filler22Type;
import com.legstar.test.coxb.redbotha.DfhcommareaType;

import junit.framework.TestCase;

public class MarshalRedbothaTest extends TestCase {

	private final static String SCHEMA_NAME = "redbotha";
	
	public void testRedbotha() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		dfhcommareaType.setCNumeric(5);

		assertEquals("0005",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 2));
	}

	public void testRedbothaSecondChoice() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		Filler22Type filler22 = new Filler22Type();
		filler22.setCLeftByte("A");
		filler22.setCRightByte("B");
		dfhcommareaType.setFiller22(filler22);

		assertEquals("c1c2",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 2));
	}
}
