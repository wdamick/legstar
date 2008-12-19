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
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;
import com.legstar.test.coxb.numzoned.ObjectFactory;
import com.legstar.test.coxb.numzoned.Dfhcommarea;

public class MarshalNumzonedTest extends TestCase {

	public void testNumzoned() throws HostException{
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[13];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);
		// Create an instance of the JAXB object factory
		ObjectFactory objectFactory = new ObjectFactory();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, getNumzoned(objectFactory));
		ccem.accept(mv);
		//		              <><--><----><--><--><---->
		//		              1 1 2 1 2 3 1 2 1 2 1 2 3  
		//		              6   -5 -7 8   +1 + 9 1 1 - 
				assertEquals("f6f0d5d0f7f8f0c14ef9f1f160",HostData.toHexString(hostBytes));
	}
	
	private Object getNumzoned(ObjectFactory objectFactory) {
		// Create and populate an instance of an object (JAXB annotated)
		Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
		dfhcommarea.setLU(6);
		dfhcommarea.setLS(Short.parseShort("-5"));
		dfhcommarea.setLSSignL(Short.parseShort("-78"));
		dfhcommarea.setLSSignT(Short.parseShort("1"));
		dfhcommarea.setLSSignSL(Short.parseShort("9"));
		dfhcommarea.setLSSignST(Short.parseShort("-11"));
		return dfhcommarea;
	}

}
