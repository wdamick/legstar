package com.legstar.test.coxb;



import com.legstar.host.HostData;
import com.legstar.test.coxb.floatmix.DfhcommareaType;

import junit.framework.TestCase;

public class UnmarshalFloatmixTest extends TestCase {

	public void testFloatmix() throws Exception {

		String hexString   = "434d2000000000004110000045543ae9361677a460ffffff000000000000000000000000000000000000000000000000";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "floatmix");
		
		assertEquals(0f,dfhcommareaType.getCFloat0());
		assertEquals(1f,dfhcommareaType.getCFloat1());
		assertEquals(1234f,dfhcommareaType.getCFloat1234());
		assertEquals(345006.56779999996f,dfhcommareaType.getCFloat345006P5678());
		assertEquals(3.40282347E+38f,dfhcommareaType.getCFloat3P40282347Ep38());
		assertEquals(7.982005E-14f,dfhcommareaType.getCFloat798P20067Em16());
	}
}
