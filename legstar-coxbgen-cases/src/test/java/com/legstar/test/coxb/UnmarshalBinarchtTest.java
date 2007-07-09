package com.legstar.test.coxb;



import java.math.BigInteger;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.binarcht.DfhcommareaType;

import junit.framework.TestCase;

public class UnmarshalBinarchtTest extends TestCase {

	public void testBinarcht() throws Exception {

		//		              <--><--><------><------><--------------><--------------><--><--><------><------><--------------><-------------->
		//		              1 2 1 2 1 2 3 4 1 2 3 4 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 1 2 1 2 3 4 1 2 3 4 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 
		//		                 06553       042949672                1844674407370955-3273276-123456721474836-1234567890123451234567890123456         
		String hexString   = "0000ffff00000000ffffffff0000000000000000ffffffffffffffff80007ffff8a432eb7fffffffffd423aba294b479002bdc545d6b4b87";
		byte[] hostBytes = HostData.toByteArray(hexString);

		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "binarcht");
		
		assertEquals(12345678901234567l, dfhcommareaType.getLsSignedNative().getLsPs9X18Max());
		assertEquals(-12345678901234567l, dfhcommareaType.getLsSignedNative().getLsPs9X18Min());
		assertEquals(32767, dfhcommareaType.getLsSignedNative().getLsPs9X4Max());
		assertEquals(-32768, dfhcommareaType.getLsSignedNative().getLsPs9X4Min());
		assertEquals(2147483647, dfhcommareaType.getLsSignedNative().getLsPs9X9Max());
		assertEquals(-123456789, dfhcommareaType.getLsSignedNative().getLsPs9X9Min());
		
		assertEquals(65535, dfhcommareaType.getLsUnsignedNative().getLsP9X4Max());
		assertEquals(0, dfhcommareaType.getLsUnsignedNative().getLsP9X4Min());
		assertEquals(4294967295l, dfhcommareaType.getLsUnsignedNative().getLsP9X9Max());
		assertEquals(0l, dfhcommareaType.getLsUnsignedNative().getLsP9X9Min());
		assertEquals(new BigInteger("18446744073709551615"), dfhcommareaType.getLsUnsignedNative().getLsP9X18Max());
		assertEquals(new BigInteger("0"), dfhcommareaType.getLsUnsignedNative().getLsP9X18Min());
	}
}
