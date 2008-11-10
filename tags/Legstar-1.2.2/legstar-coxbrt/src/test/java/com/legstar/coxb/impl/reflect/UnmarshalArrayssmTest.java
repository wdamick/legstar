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
import com.legstar.test.coxb.arrayssm.ObjectFactory;
import com.legstar.test.coxb.arrayssm.Dfhcommarea;
import com.legstar.util.JaxbUtil;

public class UnmarshalArrayssmTest extends TestCase {
	/**
	 * Unmarshal Arrayssm.
	 * @throws HostException if anything goes wrong
	 * @throws ClassNotFoundException 
	 */
	public final void testArrayssm() throws HostException, ClassNotFoundException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		//		            <----------><----------------------------><------------------------------------------------------><-------->
		//		            1 2 3 4 5 6 1 2 3 4 5 6 7 8 9 1011121314151 2 3 4 5 6 7 8 9 101112131415161718192021222324252627281 2 3 4 5 
		//		            T S 1 T S 2 T C E C 1 T C E C 2 T C E C 3 T C 2 E C 2 1 T C 2 E C 2 2 T C 2 E C 2 3 T C 2 E C 2 4 1 2 3 4 5
		String hexString = "e3e2f1e3e2f2e3c3c5c3f1e3c3c5c3f2e3c3c5c3f3e3c3f2c5c3f2f1e3c3f2c5c3f2f2e3c3f2c5c3f2f3e3c3f2c5c3f2f4f1f2f3f4f5";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		ObjectFactory objectFactory = new ObjectFactory();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory,
				JaxbUtil.loadClass("com.legstar.test.coxb.arrayssm.Dfhcommarea"));
		ccem.accept(uv);
		Dfhcommarea dfhcommarea = (Dfhcommarea) ccem.getObjectValue(Dfhcommarea.class);
		
		assertEquals("TS1", dfhcommarea.getTableSimple().get(0));
		assertEquals("TS2", dfhcommarea.getTableSimple().get(1));
		assertEquals("TCEC1", dfhcommarea.getTableComplex().get(0).getElementComplex());
		assertEquals("TCEC2", dfhcommarea.getTableComplex().get(1).getElementComplex());
		assertEquals("TCEC3", dfhcommarea.getTableComplex().get(2).getElementComplex());
		assertEquals("TC2EC21", dfhcommarea.getTableComplex2().getElementComplex2().get(0));
		assertEquals("TC2EC22", dfhcommarea.getTableComplex2().getElementComplex2().get(1));
		assertEquals("TC2EC23", dfhcommarea.getTableComplex2().getElementComplex2().get(2));
		assertEquals("TC2EC24", dfhcommarea.getTableComplex2().getElementComplex2().get(3));
		assertEquals("1", dfhcommarea.getTableSimpleNumeric().get(0).toString());
		assertEquals("2", dfhcommarea.getTableSimpleNumeric().get(1).toString());
		assertEquals("3", dfhcommarea.getTableSimpleNumeric().get(2).toString());
		assertEquals("4", dfhcommarea.getTableSimpleNumeric().get(3).toString());
		assertEquals("5", dfhcommarea.getTableSimpleNumeric().get(4).toString());
	}

}
