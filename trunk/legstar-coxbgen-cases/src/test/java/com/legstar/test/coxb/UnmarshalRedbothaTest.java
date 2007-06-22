package com.legstar.test.coxb;



import com.legstar.host.HostData;
import com.legstar.test.coxb.redbotha.DfhcommareaType;

import junit.framework.TestCase;

public class UnmarshalRedbothaTest extends TestCase {

	public void testRedbothaBothChoice() throws Exception {

		String hexString   = "c1c2";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "redbotha");

		assertEquals(49602,dfhcommareaType.getCNumeric().intValue());
		assertEquals("A",dfhcommareaType.getFiller22().getCLeftByte());
		assertEquals("B",dfhcommareaType.getFiller22().getCRightByte());
	}
}
