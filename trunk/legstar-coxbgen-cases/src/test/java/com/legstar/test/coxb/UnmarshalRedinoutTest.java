package com.legstar.test.coxb;



import com.legstar.host.HostData;
import com.legstar.test.coxb.redinout.DfhcommareaType;

import junit.framework.TestCase;

public class UnmarshalRedinoutTest extends TestCase {

	public void testRedinout() throws Exception {

		String hexString   = "0023f1f2f3f4f5f6f7f8c1c2c3c4c5c1c2c3c4c5c1c2c3c4c5d5c2";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "redinout");
		
		assertEquals(35,dfhcommareaType.getCNumeric());
		assertEquals(12345678,dfhcommareaType.getCParaout().getCSomeOutput());
		assertEquals(null,dfhcommareaType.getCParain());
	}

}
