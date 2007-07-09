package com.legstar.test.coxb;



import com.legstar.coxb.host.HostData;

import junit.framework.TestCase;
import com.legstar.test.coxb.fixarcom.DfhcommareaType;

public class UnmarshalFixarcomTest extends TestCase {

	public void testFixarcom() throws Exception {

		//		              <------------><------------><------------><------------><------------><------------><------------>
		//		              1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 
		//		              A B J A 0    0A B J A 1    7A B J A 0   14A B J A 3   21A B J A 4   28A B J A 5   35A B J A 6   42             
		String hexString   = "c1c2d1c1f00000c1c2d1c1f10007c1c2d1c1f2000ec1c2d1c1f30015c1c2d1c1f4001cc1c2d1c1f50023c1c2d1c1f6002a";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "fixarcom");
		
		for(int i=0;i< 7;i++) {
			com.legstar.test.coxb.fixarcom.CArrayType item = dfhcommareaType.getCArray().get(i);
			assertEquals("ABJA" + Integer.toString(i),item.getCItem1());
			assertEquals(Short.parseShort(Integer.toString(7 * i)),item.getCItem2());
		}
	}
}
