package com.legstar.test.coxb;



import java.math.BigInteger;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.binpkdus.DfhcommareaType;

import junit.framework.TestCase;

public class UnmarshalBinpkdusTest extends TestCase {

	public void testBinpkdus() throws Exception {

		String hexString   = "0f3f012f0032769f0123456789012345678f1234567890123456789f1234567890123456789012345678901f000000000000000000000000";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "binpkdus");
		
		assertEquals(3,dfhcommareaType.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X1());
		assertEquals(123456789012345678l,dfhcommareaType.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X18());
		assertEquals(0,dfhcommareaType.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X1Null());
		assertEquals(12,dfhcommareaType.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X2());
		assertEquals(32769,dfhcommareaType.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X7());
		assertEquals(new BigInteger("1234567890123456789"),dfhcommareaType.getLsUnsignedPackedDecimal().getLsExtend().getLsP9X19());
		assertEquals(new BigInteger("1234567890123456789012345678901"),dfhcommareaType.getLsUnsignedPackedDecimal().getLsExtend().getLsP9X31());
	}
}
