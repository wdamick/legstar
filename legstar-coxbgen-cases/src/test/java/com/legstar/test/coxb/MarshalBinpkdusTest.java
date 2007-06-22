package com.legstar.test.coxb;




import java.math.BigInteger;

import com.legstar.test.coxb.binpkdus.LsCompatType;
import com.legstar.test.coxb.binpkdus.LsExtendType;
import com.legstar.test.coxb.binpkdus.LsUnsignedPackedDecimalType;
import com.legstar.test.coxb.binpkdus.DfhcommareaType;

import junit.framework.TestCase;

public class MarshalBinpkdusTest extends TestCase {

	private final static String SCHEMA_NAME = "binpkdus";

	public void testBinpkdus() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		
		LsCompatType lsCompatType = new LsCompatType();
		lsCompatType.setLsP9X1(3);
		lsCompatType.setLsP9X18(123456789012345678l);
		lsCompatType.setLsP9X1Null(0);
		lsCompatType.setLsP9X2(12);
		lsCompatType.setLsP9X7(32769);
		
		LsExtendType lsExtendType = new LsExtendType();
		lsExtendType.setLsP9X19(new BigInteger("1234567890123456789"));
		lsExtendType.setLsP9X31(new BigInteger("1234567890123456789012345678901"));
		
		LsUnsignedPackedDecimalType lsUnsignedPackedDecimalType = new LsUnsignedPackedDecimalType();
		lsUnsignedPackedDecimalType.setLsCompat(lsCompatType);
		lsUnsignedPackedDecimalType.setLsExtend(lsExtendType);
		dfhcommareaType.setLsUnsignedPackedDecimal(lsUnsignedPackedDecimalType);
		
		assertEquals("0f3f012f0032769f0123456789012345678f1234567890123456789f1234567890123456789012345678901f000000000000000000000000",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 56));
	}
}
