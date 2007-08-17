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
import com.legstar.test.coxb.redsimpt.ObjectFactory;
import com.legstar.test.coxb.redsimpt.DfhcommareaType;

public class UnmarshalRedsimptTest extends TestCase {
	/**
	 * Unmarshal Redsimpt.
	 * @throws HostException if anything goes wrong
	 * @throws ClassNotFoundException 
	 */
	public final void testRedsimpt() throws HostException, ClassNotFoundException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);

		// Create an instance of the JAXB object factory
		ObjectFactory objectFactory = new ObjectFactory();

		/* Unmarshal from non-numeric data */
		byte[] hostBytes = HostData.toByteArray("f0f1f2f3f4f5f6f7f8c1f1f2f3f4f5f6f7f8");
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory,
				Class.forName("com.legstar.test.coxb.redsimpt.DfhcommareaType"));
		ccem.accept(uv);
		DfhcommareaType dfhcommarea = (DfhcommareaType) ccem.getObjectValue(DfhcommareaType.class);
		assertEquals("012345678A12345678", dfhcommarea.getCDefinition1());
		
		/* Unmarshal from numeric data */
		hostBytes = HostData.toByteArray("f8f7f6f5f4f3f2f1f9f8f7f6f5f4f3f2f1f0");
		uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		dfhcommarea = objectFactory.createDfhcommareaType();
		ccem = new CComplexReflectBinding(objectFactory,
				dfhcommarea);
		ccem.accept(uv);
		assertEquals(new Long(876543219876543210L), dfhcommarea.getCDefinition2());
		
	}

}
