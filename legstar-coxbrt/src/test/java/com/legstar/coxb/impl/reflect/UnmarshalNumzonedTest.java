/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.coxb.impl.reflect;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.visitor.CobolUnmarshalVisitor;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

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
