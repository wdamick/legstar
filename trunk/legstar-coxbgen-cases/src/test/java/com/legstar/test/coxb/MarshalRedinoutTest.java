package com.legstar.test.coxb;

import com.legstar.test.coxb.redinout.DfhcommareaType;
import com.legstar.test.coxb.redinout.CParainType;

import junit.framework.TestCase;

public class MarshalRedinoutTest extends TestCase {

	private final static String SCHEMA_NAME = "redinout";
	
	public void testRedinout() throws Exception{
		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		
		dfhcommareaType.setCNumeric(35);
		CParainType parain = new CParainType();
		parain.setCSomeInput("ABCDEABCDEABCDE");
		dfhcommareaType.setCParain(parain);

		assertEquals("0023c1c2c3c4c5c1c2c3c4c5c1c2c3c4c5",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 17));
	}

}
