package com.legstar.test.coxb;




import java.math.BigInteger;

import com.legstar.test.coxb.binnatus.LsUnsignedNativeType;
import com.legstar.test.coxb.binnatus.LsDoublewordsType;
import com.legstar.test.coxb.binnatus.LsFullwordsType;
import com.legstar.test.coxb.binnatus.LsHalfwordsType;
import com.legstar.test.coxb.binnatus.DfhcommareaType;

import junit.framework.TestCase;

public class MarshalBinnatusTest extends TestCase {

	private final static String SCHEMA_NAME = "binnatus";

	public void testBinnatus() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		
		LsUnsignedNativeType lsUnsignedNativeType = new LsUnsignedNativeType(); 
		
		LsHalfwordsType lsHalfwordsType = new LsHalfwordsType();
		lsHalfwordsType.setLsP9X4High(32769);
		lsHalfwordsType.setLsP9X4Low(127);
		lsHalfwordsType.setLsP9X4Max(65535);
		lsHalfwordsType.setLsP9X4Min(0);
		
		LsFullwordsType lsFullwordsType = new LsFullwordsType();
		lsFullwordsType.setLsP9X9High(2147483649l);
		lsFullwordsType.setLsP9X9Low(65534);
		lsFullwordsType.setLsP9X9Max(4294967295l);
		lsFullwordsType.setLsP9X9Min(0);
		
		LsDoublewordsType lsDoublewordsType = new LsDoublewordsType();
		lsDoublewordsType.setLsP9X18High(new BigInteger("18446744069414584318"));
		lsDoublewordsType.setLsP9X18Low(new BigInteger("4294967294"));
		lsDoublewordsType.setLsP9X18Max(new BigInteger("18446744073709551615"));
		lsDoublewordsType.setLsP9X18Min(new BigInteger("0"));
		
		lsUnsignedNativeType.setLsHalfwords(lsHalfwordsType);
		lsUnsignedNativeType.setLsFullwords(lsFullwordsType);
		lsUnsignedNativeType.setLsDoublewords(lsDoublewordsType);
		
		dfhcommareaType.setLsUnsignedNative(lsUnsignedNativeType);
		
		assertEquals("0000007f8001ffff000000000000fffe80000001ffffffff000000000000000000000000fffffffefffffffefffffffeffffffffffffffff",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 56));
	}
}
