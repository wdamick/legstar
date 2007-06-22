package com.legstar.test.coxb;



import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolUnmarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.host.HostData;
import com.legstar.host.HostException;
import com.legstar.test.coxb.charsets.ObjectFactory;
import com.legstar.test.coxb.charsets.bind.DfhcommareaTypeBinding;
import com.legstar.test.coxb.charsets.DfhcommareaType;

import junit.framework.TestCase;

public class UnmarshalCharsetsTest extends TestCase {

	public void testTypesmix() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		cobolContext.setHostCharsetName("IBM01147");
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		String hexString = "e08140837d85a2a340a495409799968293d094854040404040404040404040400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e9006c00e9006d0065006e00740061006900720065002000e00020007200e90073006f00750064007200650020002000200020002000200020002000200020";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		ObjectFactory objectFactory = new ObjectFactory();

		// Traverse the object structure, visiting each node with the visitor
		DfhcommareaTypeBinding ccem = new DfhcommareaTypeBinding(objectFactory);
		ccem.accept(uv);
		DfhcommareaType dfhcommarea = ccem.getDfhcommareaType();
		
		assertEquals("ça c'est un problème            ",dfhcommarea.getComLocal());
		assertEquals("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",HostData.toHexString(dfhcommarea.getComDbcs()));
		assertEquals("élémentaire à résoudre          ",dfhcommarea.getComNational());
	}
}
