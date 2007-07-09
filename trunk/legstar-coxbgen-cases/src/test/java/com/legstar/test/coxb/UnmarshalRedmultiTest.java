package com.legstar.test.coxb;



import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.redmulti.DfhcommareaType;

import junit.framework.TestCase;

public class UnmarshalRedmultiTest extends TestCase {

	public void testRedmultiNormal() throws Exception {
		String hexString = "959699948193c1c2d1c1c4c8c1d6e4c1e9404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "redmulti");
		
		assertEquals("normal",dfhcommareaType.getCOutputType());
		assertEquals("ABJADHAOUAZ",dfhcommareaType.getFiller35().getCString());
	}

	public void testRedmultiError() throws Exception {
		String hexString = "859999969940f0f0f7f5c1c2d6d4c9d5c1c2d3c5404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "redmulti");
		
		assertEquals("error",dfhcommareaType.getCOutputType());
		assertEquals(75, dfhcommareaType.getFiller38().getCErrorNum());
		assertEquals("ABOMINABLE",dfhcommareaType.getFiller38().getCErrorDescription());
	}

}
