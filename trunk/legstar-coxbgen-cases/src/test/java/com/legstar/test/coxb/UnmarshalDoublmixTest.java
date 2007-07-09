package com.legstar.test.coxb;



import com.legstar.coxb.host.HostData;

import junit.framework.TestCase;
import com.legstar.test.coxb.doublmix.DfhcommareaType;

public class UnmarshalDoublmixTest extends TestCase {

	public void testDoublmix() throws Exception {

		String hexString   = "434d2000000000000000000000000000411000000000000045543ae915b573e0361677a4590fab6060ffffff048ff9e0";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "doublmix");
		
		assertEquals(0d,dfhcommareaType.getCDouble0());
		assertEquals(1d,dfhcommareaType.getCDouble1());
		assertEquals(1234d,dfhcommareaType.getCDouble1234());
		assertEquals(345006.56779999984d,dfhcommareaType.getCDouble345006P5678());
		assertEquals(3.4028234699999995E+38,dfhcommareaType.getCDouble3P40282347Ep38());
		assertEquals(7.982006699999985E-14,dfhcommareaType.getCDouble798P20067Em16());
	}
}
