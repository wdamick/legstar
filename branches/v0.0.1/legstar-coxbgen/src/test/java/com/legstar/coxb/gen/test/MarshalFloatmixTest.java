/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.coxb.gen.test;


import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolMarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;

public class MarshalFloatmixTest extends TestCase {

	public void testFloatmix() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[48];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.floatmix.ObjectFactory objectFactory = new com.legstar.test.coxb.floatmix.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.floatmix.DfhcommareaType dfhcommareaType = objectFactory.createDfhcommareaType();

		dfhcommareaType.setCFloat0(0f);
		dfhcommareaType.setCFloat1(1f);
		dfhcommareaType.setCFloat1234(1234f);
		dfhcommareaType.setCFloat345006P5678(345006.5678f);
		dfhcommareaType.setCFloat3P40282347Ep38(3.40282347E+38f);
		dfhcommareaType.setCFloat798P20067Em16(798.20067E-16f);
		

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.floatmix.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.floatmix.bind.DfhcommareaTypeBinding(objectFactory, dfhcommareaType);
		ccem.accept(mv);
		assertEquals("434d2000000000004110000045543ae9361677a460ffffff000000000000000000000000000000000000000000000000",HostData.toHexString(hostBytes));
	}
}
