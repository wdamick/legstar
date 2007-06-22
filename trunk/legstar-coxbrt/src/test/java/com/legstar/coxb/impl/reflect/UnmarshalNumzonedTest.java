package com.legstar.coxb.impl.reflect;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolUnmarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;
import com.legstar.test.coxb.numzoned.ObjectFactory;
import com.legstar.test.coxb.numzoned.DfhcommareaType;

public class UnmarshalNumzonedTest extends TestCase {
	/**
	 * Unmarshal Numzoned.
	 * @throws HostException if anything goes wrong
	 * @throws ClassNotFoundException 
	 */
	public final void testNumzoned() throws HostException, ClassNotFoundException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		//		            <><--><----><--><--><---->
		//		            1 1 2 1 2 3 1 2 1 2 1 2 3  
		//		            6   -5 -7 8   +1 + 9 1 1 - 
		String hexString = "f6f0d5d0f7f8f0c14ef9f1f160";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		ObjectFactory objectFactory = new ObjectFactory();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory,
				Class.forName("com.legstar.test.coxb.numzoned.DfhcommareaType"));
		ccem.accept(uv);
		DfhcommareaType dfhcommarea = (DfhcommareaType) ccem.getObjectValue(DfhcommareaType.class);
		
		assertEquals(6, dfhcommarea.getLU());
		assertEquals(-5, dfhcommarea.getLS());
		assertEquals(-78, dfhcommarea.getLSSignL());
		assertEquals(1, dfhcommarea.getLSSignT());
		assertEquals(9, dfhcommarea.getLSSignSL());
		assertEquals(-11, dfhcommarea.getLSSignST());
	}

}
