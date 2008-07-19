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
import com.legstar.coxb.visitor.CobolUnmarshalVisitor;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;
import com.legstar.test.coxb.arraysdo.ObjectFactory;
import com.legstar.test.coxb.arraysdo.DfhcommareaType;
import com.legstar.util.JaxbUtil;

public class UnmarshalArraysdoTest extends TestCase {
	/**
	 * Unmarshal Arraysdo.
	 * @throws HostException if anything goes wrong
	 * @throws ClassNotFoundException 
	 */
	public final void testArraysdo() throws HostException, ClassNotFoundException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		//		            <--><--------><--------><--------><--------><-------->
		//		            1 2 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 
		//		            0 5 O D O 0 1 O D O 0 2 O D O 0 3 O D O 0 4 O D O 0 5 
		String hexString = "f0f5d6c4d6f0f1d6c4d6f0f2d6c4d6f0f3d6c4d6f0f4d6c4d6f0f5";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		ObjectFactory objectFactory = new ObjectFactory();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory,
				JaxbUtil.loadClass("com.legstar.test.coxb.arraysdo.DfhcommareaType"));
		ccem.accept(uv);
		DfhcommareaType dfhcommarea = (DfhcommareaType) ccem.getObjectValue(DfhcommareaType.class);
		
		assertEquals(5, dfhcommarea.getTableSize());
		assertEquals("ODO01", dfhcommarea.getTableOdo().get(0));
		assertEquals("ODO02", dfhcommarea.getTableOdo().get(1));
		assertEquals("ODO03", dfhcommarea.getTableOdo().get(2));
		assertEquals("ODO04", dfhcommarea.getTableOdo().get(3));
		assertEquals("ODO05", dfhcommarea.getTableOdo().get(4));
	}

}
