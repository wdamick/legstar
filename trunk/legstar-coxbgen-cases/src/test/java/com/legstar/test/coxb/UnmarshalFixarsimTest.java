package com.legstar.test.coxb;



import com.legstar.host.HostData;
import com.legstar.test.coxb.fixarsim.DfhcommareaType;

import junit.framework.TestCase;

public class UnmarshalFixarsimTest extends TestCase {

	public void testFixarsim() throws Exception {

		//		              <---------------------------->
		//		              1 2 3 4 5 6 7 8 9 101112131415
		//		              P R E M I D E U X I T R O I S             
		String hexString   = "d7d9c5d4c9c4c5e4e7c9e3d9d6c9e2";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "fixarsim");
		
		assertEquals("PREMI",dfhcommareaType.getCArray().get(0).trim());
		assertEquals("DEUXI",dfhcommareaType.getCArray().get(1).trim());
		assertEquals("TROIS",dfhcommareaType.getCArray().get(2).trim());
	}
}
