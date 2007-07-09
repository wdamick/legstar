package com.legstar.test.coxb;



import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.numzoned.DfhcommareaType;

import junit.framework.TestCase;

public class UnmarshalNumzonedTest extends TestCase {

	public void testNumzoned() throws Exception {

		//		            <><--><----><--><--><---->
		//		            1 1 2 1 2 3 1 2 1 2 1 2 3  
		//		            6   -5 -7 8   +1 + 9 1 1 - 
		String hexString = "f6f0d5d0f7f8f0c14ef9f1f160";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "numzoned");
		
		assertEquals(6, dfhcommareaType.getLU());
		assertEquals(-5, dfhcommareaType.getLS());
		assertEquals(-78, dfhcommareaType.getLSSignL());
		assertEquals(1, dfhcommareaType.getLSSignT());
		assertEquals(9, dfhcommareaType.getLSSignSL());
		assertEquals(-11, dfhcommareaType.getLSSignST());
	}
}
