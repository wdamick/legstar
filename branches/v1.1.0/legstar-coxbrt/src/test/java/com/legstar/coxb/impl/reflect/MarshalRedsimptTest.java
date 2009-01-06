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
import com.legstar.coxb.visitor.CobolMarshalVisitor;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;
import com.legstar.test.coxb.redsimpt.ObjectFactory;
import com.legstar.test.coxb.redsimpt.DfhcommareaType;

public class MarshalRedsimptTest extends TestCase {

	public void testRedsimpt() throws HostException{
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[18];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);
		// Create an instance of the JAXB object factory
		ObjectFactory objectFactory = new ObjectFactory();

		// Marshal with alternative 1
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, getRedsimptAlternative1(objectFactory));
		ccem.accept(mv);
		assertEquals("f0f1f2f3f4f5f6f7f8c1f1f2f3f4f5f6f7f8",HostData.toHexString(hostBytes));

		// Marshal with alternative 2
		mv = new CobolMarshalVisitor(hostBytes, 0, cc);
		ccem = new CComplexReflectBinding(objectFactory, getRedsimptAlternative2(objectFactory));
		ccem.accept(mv);
		assertEquals("f8f7f6f5f4f3f2f1f9f8f7f6f5f4f3f2f1f0",HostData.toHexString(hostBytes));
	}
	
	private Object getRedsimptAlternative1(ObjectFactory objectFactory) {
		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();
		dfhcommarea.setCDefinition1("012345678A12345678");
		return dfhcommarea;
	}

	private Object getRedsimptAlternative2(ObjectFactory objectFactory) {
		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();
		dfhcommarea.setCDefinition2(876543219876543210L);
		return dfhcommarea;
	}
}