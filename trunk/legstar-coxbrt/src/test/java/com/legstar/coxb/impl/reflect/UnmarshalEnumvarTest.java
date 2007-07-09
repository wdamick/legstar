package com.legstar.coxb.impl.reflect;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.visitor.CobolUnmarshalVisitor;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;
import com.legstar.test.coxb.enumvar.ObjectFactory;
import com.legstar.test.coxb.enumvar.SearchRequestType;
import com.legstar.test.coxb.enumvar.SafeSearchOptionsType; // Enum

public class UnmarshalEnumvarTest extends TestCase {
	/**
	 * Unmarshal Enumvar.
	 * @throws HostException if anything goes wrong
	 * @throws ClassNotFoundException 
	 */
	public final void testEnumvar() throws HostException, ClassNotFoundException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		//		            <------------------------------------------------------------------------------><--------------------------------------------------------------><-->
		//		            1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 1 2
		//		            5 5 8 8 C 3 A C E 9 4 9 3 1 5 B 3 E C A A D D A 9 0 8 6 1 1 B D F 5 D 8 D 5 A A O f f                                                              4
		String hexString = "f5f5f8f8c3f3c1c3c5f9f4f9f3f1f5c2f3c5c3c1c1c4c4c1f9f0f8f6f1f1c2c4c6f5c4f8c4f5c1c1d6868640404040404040404040404040404040404040404040404040404040400004";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		ObjectFactory objectFactory = new ObjectFactory();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory,
				Class.forName("com.legstar.test.coxb.enumvar.SearchRequestType"));
		ccem.accept(uv);
		SearchRequestType searchRequest = (SearchRequestType) ccem.getObjectValue(SearchRequestType.class);
		
		assertEquals("5588C3ACE949315B3ECAADDA908611BDF5D8D5AA", searchRequest.getAppID());
		assertEquals(SafeSearchOptionsType.OFF, searchRequest.getSafeSearch());
		assertEquals("4", searchRequest.getSearchWeight().toString());
	}

}
