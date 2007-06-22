package com.legstar.test.coxb;



import java.math.BigInteger;

import com.legstar.test.coxb.binarcht.LsSignedNativeType;
import com.legstar.test.coxb.binarcht.LsUnsignedNativeType;
import com.legstar.test.coxb.binarcht.DfhcommareaType;

import junit.framework.TestCase;

public class MarshalBinarchtTest extends TestCase {
	
	private final static String SCHEMA_NAME = "binarcht";

	public void testBinarcht() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		LsSignedNativeType lsSignedNativeType = new LsSignedNativeType();
		lsSignedNativeType.setLsPs9X18Max(12345678901234567l);
		lsSignedNativeType.setLsPs9X18Min(-12345678901234567l);
		lsSignedNativeType.setLsPs9X4Max(new Short("32767"));
		lsSignedNativeType.setLsPs9X4Min(new Short("-32768"));
		lsSignedNativeType.setLsPs9X9Max(2147483647);
		lsSignedNativeType.setLsPs9X9Min(-123456789);
		
		dfhcommareaType.setLsSignedNative(lsSignedNativeType);
		
		LsUnsignedNativeType lsUnsignedNativeType = new LsUnsignedNativeType();
		lsUnsignedNativeType.setLsP9X18Max(new BigInteger("18446744073709551615"));
		lsUnsignedNativeType.setLsP9X18Min(new BigInteger("0"));
		lsUnsignedNativeType.setLsP9X4Max(65535);
		lsUnsignedNativeType.setLsP9X4Min(0);
		lsUnsignedNativeType.setLsP9X9Max(4294967295l);
		lsUnsignedNativeType.setLsP9X9Min(0);

		dfhcommareaType.setLsUnsignedNative(lsUnsignedNativeType);

		//		      <--><--><------><------><--------------><--------------><--><--><------><------><--------------><-------------->
		//		      1 2 1 2 1 2 3 4 1 2 3 4 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 1 2 1 2 3 4 1 2 3 4 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 
		//		         06553       042949672                1844674407370955-3273276-123456721474836-1234567890123451234567890123456         
		assertEquals("0000ffff00000000ffffffff0000000000000000ffffffffffffffff80007ffff8a432eb7fffffffffd423aba294b479002bdc545d6b4b87",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 56));
	}
}
