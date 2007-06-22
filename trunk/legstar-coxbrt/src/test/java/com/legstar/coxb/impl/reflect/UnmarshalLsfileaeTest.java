package com.legstar.coxb.impl.reflect;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolUnmarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;
import com.legstar.test.coxb.lsfileae.ObjectFactory;
import com.legstar.test.coxb.lsfileae.DfhcommareaType;

public class UnmarshalLsfileaeTest extends TestCase {
	/**
	 * Unmarshal Lsfileae.
	 * @throws HostException if anything goes wrong
	 * @throws ClassNotFoundException 
	 */
	public final void testLsfileae() throws HostException, ClassNotFoundException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		//		            <----------><--------------------------------------><--------------------------------------><--------------><--------------><--------------><---------------->
		//		            1 2 3 4 5 6 1 2 3 4 5 6 7 8 9 10111213141516171819201 2 3 4 5 6 7 8 9 10111213141516171819201 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 9
		//		            0 0 0 1 0 0 T O T O                                 L A B A S   S T R E E T                 8 8 9 9 3 3 1 4 1 0 0 4 5 8 0 0 1 0 0 . 3 5 A   V O I R
		String hexString = "f0f0f0f1f0f0e3d6e3d640404040404040404040404040404040d3c1c2c1e240e2e3d9c5c5e34040404040404040f8f8f9f9f3f3f1f4f1f0f0f4f5f84040f0f0f1f0f04bf3f5c140e5d6c9d9404040";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		ObjectFactory objectFactory = new ObjectFactory();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory,
				DfhcommareaType.class);
		ccem.accept(uv);
		DfhcommareaType dfhcommarea = (DfhcommareaType) ccem.getObjectValue(DfhcommareaType.class);
		
		assertEquals(100, dfhcommarea.getComNumber());
		assertEquals("TOTO", dfhcommarea.getComPersonal().getComName().trim());
		assertEquals("LABAS STREET", dfhcommarea.getComPersonal().getComAddress().trim());
		assertEquals("88993314", dfhcommarea.getComPersonal().getComPhone().trim());
		assertEquals("100458", dfhcommarea.getComDate().trim());
		assertEquals("00100.35", dfhcommarea.getComAmount().trim());
		assertEquals("A VOIR", dfhcommarea.getComComment().trim());
	}

}
