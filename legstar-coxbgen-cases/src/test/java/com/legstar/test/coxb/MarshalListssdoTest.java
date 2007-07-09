package com.legstar.test.coxb;

import java.util.ArrayList;
import java.util.List;

import com.legstar.test.coxb.listssdo.DfhcommareaType;

import junit.framework.TestCase;

public class MarshalListssdoTest extends TestCase {

	private final static String SCHEMA_NAME = "listssdo";
	
	public void testListssdo() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);

		List <String> listOdo = new ArrayList <String>();
		listOdo.add("ODO01");
		listOdo.add("ODO02");
		listOdo.add("ODO03");
		listOdo.add("ODO04");
		listOdo.add("ODO05");
		dfhcommareaType.getListOdo().addAll(listOdo);
		
		//		      <------><------------------------------------------------>
		//		      1 2 3 4 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
		//		      0 0 O 5 O D O 0 1 O D O 0 2 O D O 0 3 O D O 0 4 O D O 0 5 
		assertEquals("00000005d6c4d6f0f1d6c4d6f0f2d6c4d6f0f3d6c4d6f0f4d6c4d6f0f5",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 29));
	}
}
