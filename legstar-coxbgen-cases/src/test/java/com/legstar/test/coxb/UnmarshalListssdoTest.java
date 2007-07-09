package com.legstar.test.coxb;



import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.listssdo.DfhcommareaType;

import junit.framework.TestCase;

public class UnmarshalListssdoTest extends TestCase {

	public void testListssdo() throws Exception {

		//		              <------><------------------------------------------------>
		//		              1 2 3 4 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
		//		              0 0 O 5 O D O 0 1 O D O 0 2 O D O 0 3 O D O 0 4 O D O 0 5 
		String hexString   = "00000005d6c4d6f0f1d6c4d6f0f2d6c4d6f0f3d6c4d6f0f4d6c4d6f0f5";
		byte[] hostBytes = HostData.toByteArray(hexString);

		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "listssdo");
		
		assertEquals(5, dfhcommareaType.getListOdo().size());
		assertEquals("ODO01", dfhcommareaType.getListOdo().get(0));
		assertEquals("ODO02", dfhcommareaType.getListOdo().get(1));
		assertEquals("ODO03", dfhcommareaType.getListOdo().get(2));
		assertEquals("ODO04", dfhcommareaType.getListOdo().get(3));
		assertEquals("ODO05", dfhcommareaType.getListOdo().get(4));
	}
}
