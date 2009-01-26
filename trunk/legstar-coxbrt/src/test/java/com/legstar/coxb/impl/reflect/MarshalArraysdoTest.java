/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.impl.reflect;

import java.util.ArrayList;
import java.util.List;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;
import com.legstar.test.coxb.arraysdo.ObjectFactory;
import com.legstar.test.coxb.arraysdo.Dfhcommarea;

public class MarshalArraysdoTest extends TestCase {

	public void testArraysdo() throws HostException{
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[27];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);
		// Create an instance of the JAXB object factory
		ObjectFactory objectFactory = new ObjectFactory();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, getArraysdo(objectFactory));
		ccem.accept(mv);
		//		              <--><--------><--------><--------><--------><-------->
		//		              1 2 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 
		//		              0 5 O D O 0 1 O D O 0 2 O D O 0 3 O D O 0 4 O D O 0 5 
				assertEquals("f0f5d6c4d6f0f1d6c4d6f0f2d6c4d6f0f3d6c4d6f0f4d6c4d6f0f5",HostData.toHexString(hostBytes));
	}
	
	private Object getArraysdo(ObjectFactory objectFactory) {
		// Create and populate an instance of an object (JAXB annotated)
		Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
		dfhcommarea.setTableSize(5);
		List <String> tableOdo = new ArrayList <String>();
		tableOdo.add("ODO01");
		tableOdo.add("ODO02");
		tableOdo.add("ODO03");
		tableOdo.add("ODO04");
		tableOdo.add("ODO05");
		dfhcommarea.getTableOdo().addAll(tableOdo);
		return dfhcommarea;
	}

}
