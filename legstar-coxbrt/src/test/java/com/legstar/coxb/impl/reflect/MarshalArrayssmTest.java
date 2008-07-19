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

import java.util.ArrayList;
import java.util.List;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.visitor.CobolMarshalVisitor;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;
import com.legstar.test.coxb.arrayssm.ObjectFactory;
import com.legstar.test.coxb.arrayssm.DfhcommareaType;
import com.legstar.test.coxb.arrayssm.TableComplex2Type;
import com.legstar.test.coxb.arrayssm.TableComplexType;

public class MarshalArrayssmTest extends TestCase {

	public void testArrayssm() throws HostException{
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[54];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);
		// Create an instance of the JAXB object factory
		ObjectFactory objectFactory = new ObjectFactory();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, getArrayssm(objectFactory));
		ccem.accept(mv);
		//		              <----------><----------------------------><------------------------------------------------------><-------->
		//		              1 2 3 4 5 6 1 2 3 4 5 6 7 8 9 1011121314151 2 3 4 5 6 7 8 9 101112131415161718192021222324252627281 2 3 4 5 
		//		              T S 1 T S 2 T C E C 1 T C E C 2 T C E C 3 T C 2 E C 2 1 T C 2 E C 2 2 T C 2 E C 2 3 T C 2 E C 2 4 1 2 3 4 5
				assertEquals("e3e2f1e3e2f2e3c3c5c3f1e3c3c5c3f2e3c3c5c3f3e3c3f2c5c3f2f1e3c3f2c5c3f2f2e3c3f2c5c3f2f3e3c3f2c5c3f2f4f1f2f3f4f5",HostData.toHexString(hostBytes));
	}
	
	private Object getArrayssm(ObjectFactory objectFactory) {
		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();
		List <String> tableSimple = new ArrayList <String>();
		tableSimple.add("TS1");
		tableSimple.add("TS2");
		dfhcommarea.getTableSimple().addAll(tableSimple);
		
		TableComplexType tc1 = objectFactory.createTableComplexType();
		tc1.setElementComplex("TCEC1");
		dfhcommarea.getTableComplex().add(tc1);
		TableComplexType tc2 = objectFactory.createTableComplexType();
		tc2.setElementComplex("TCEC2");
		dfhcommarea.getTableComplex().add(tc2);
		TableComplexType tc3 = objectFactory.createTableComplexType();
		tc3.setElementComplex("TCEC3");
		dfhcommarea.getTableComplex().add(tc3);
		
		TableComplex2Type tcc = objectFactory.createTableComplex2Type();
		tcc.getElementComplex2().add("TC2EC21");
		tcc.getElementComplex2().add("TC2EC22");
		tcc.getElementComplex2().add("TC2EC23");
		tcc.getElementComplex2().add("TC2EC24");
		dfhcommarea.setTableComplex2(tcc);

		List <Integer> tableSimpleNumeric = new ArrayList <Integer>();
		tableSimpleNumeric.add(1);
		tableSimpleNumeric.add(2);
		tableSimpleNumeric.add(3);
		tableSimpleNumeric.add(4);
		tableSimpleNumeric.add(5);
		dfhcommarea.getTableSimpleNumeric().addAll(tableSimpleNumeric);
		return dfhcommarea;
	}

}
