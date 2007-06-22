package com.legstar.test.coxb;



import com.legstar.host.HostData;
import com.legstar.test.coxb.binnatsi.DfhcommareaType;

import junit.framework.TestCase;

public class UnmarshalBinnatsiTest extends TestCase {

	public void testBinnatsi() throws Exception {

		String hexString   = "8000ff8004157fff80000000ffffff80075bcd157fffffff8000000000000000ffffffff0000000200000003ffffffff7fffffffffffffff";
		byte[] hostBytes = HostData.toByteArray(hexString);

		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "binnatsi");
		
		assertEquals(1045, dfhcommareaType.getLsUnsignedNative().getLsHalfwords().getLsPs9X4High());
		assertEquals(-128, dfhcommareaType.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Low());
		assertEquals(32767, dfhcommareaType.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Max());
		assertEquals(-32768, dfhcommareaType.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Min());
		
		assertEquals(123456789, dfhcommareaType.getLsUnsignedNative().getLsFullwords().getLsPs9X9High());
		assertEquals(-128, dfhcommareaType.getLsUnsignedNative().getLsFullwords().getLsPs9X9Low());
		assertEquals(2147483647, dfhcommareaType.getLsUnsignedNative().getLsFullwords().getLsPs9X9Max());
		assertEquals(-2147483648, dfhcommareaType.getLsUnsignedNative().getLsFullwords().getLsPs9X9Min());
		
		assertEquals(17179869183l, dfhcommareaType.getLsUnsignedNative().getLsDoublewords().getLsPs9X18High());
		assertEquals(-4294967294l, dfhcommareaType.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Low());
		assertEquals(9223372036854775807l, dfhcommareaType.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Max());
		assertEquals(-9223372036854775808l, dfhcommareaType.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Min());
	}
}
