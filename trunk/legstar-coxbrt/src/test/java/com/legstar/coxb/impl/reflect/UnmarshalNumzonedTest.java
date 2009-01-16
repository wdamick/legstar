/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.impl.reflect;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.visitor.CobolUnmarshalVisitor;
import com.legstar.coxb.util.Utils;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;
import com.legstar.test.coxb.numzoned.ObjectFactory;
import com.legstar.test.coxb.numzoned.Dfhcommarea;

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
		        Utils.loadClass("com.legstar.test.coxb.numzoned.Dfhcommarea"));
		ccem.accept(uv);
		Dfhcommarea dfhcommarea = (Dfhcommarea) ccem.getObjectValue(Dfhcommarea.class);
		
		assertEquals(6, dfhcommarea.getLU());
		assertEquals(-5, dfhcommarea.getLS());
		assertEquals(-78, dfhcommarea.getLSSignL());
		assertEquals(1, dfhcommarea.getLSSignT());
		assertEquals(9, dfhcommarea.getLSSignSL());
		assertEquals(-11, dfhcommarea.getLSSignST());
	}

}
