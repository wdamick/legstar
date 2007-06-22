package com.legstar.test.coxb;



import com.legstar.test.coxb.doublmix.DfhcommareaType;

import junit.framework.TestCase;

public class MarshalDoublmixTest extends TestCase {

	private final static String SCHEMA_NAME = "doublmix";
	
	public void testDoublmix() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		dfhcommareaType.setCDouble0(0d);
		dfhcommareaType.setCDouble1(1d);
		dfhcommareaType.setCDouble1234(1234d);
		dfhcommareaType.setCDouble345006P5678(345006.5678d);
		dfhcommareaType.setCDouble3P40282347Ep38(3.40282347E+38);
		dfhcommareaType.setCDouble798P20067Em16(798.20067E-16);
		
		assertEquals("434d2000000000000000000000000000411000000000000045543ae915b573e0361677a4590fab6060ffffff048ff9e0",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 48));
	}
}
