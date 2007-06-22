package com.legstar.test.coxb;



import com.legstar.host.HostData;
import com.legstar.test.coxb.redsimpt.DfhcommareaType;

import junit.framework.TestCase;

public class UnmarshalRedsimptTest extends TestCase {

	public void testRedsimpt() throws Exception {

		//		           <------------------------------------>
		//		            1 2 3 4 5 6 7 8 9 101112131415161718
		//		            A B C D E F G H I J K L M N O       
		String hexString = "c1c2c3c4c5c6c7c8c9d1d2d3d4d5d6404040";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "redsimpt");
		
		assertEquals("ABCDEFGHIJKLMNO   ",dfhcommareaType.getCDefinition1());
	}

	public void testRedsimptSecondChoice() throws Exception {

		//		           <------------------------------------>
		//		            1 2 3 4 5 6 7 8 9 101112131415161718
		//		            0 0 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5       
		String hexString = "f0f0f0f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "redsimpt");
		
		assertEquals(123456789012345l,dfhcommareaType.getCDefinition2().longValue());
	}
}
