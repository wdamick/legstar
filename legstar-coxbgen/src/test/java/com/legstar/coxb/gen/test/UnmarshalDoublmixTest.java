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
package com.legstar.coxb.gen.test;


import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolUnmarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;

public class UnmarshalDoublmixTest extends TestCase {

	public void testDoublmix() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		String hexString   = "434d2000000000000000000000000000411000000000000045543ae915b573e0361677a4590fab6060ffffff048ff9e0";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.doublmix.ObjectFactory objectFactory = new com.legstar.test.coxb.doublmix.ObjectFactory();

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.doublmix.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.doublmix.bind.DfhcommareaTypeBinding(objectFactory);
		ccem.accept(uv);
		com.legstar.test.coxb.doublmix.DfhcommareaType dfhcommareaType = ccem.getJaxbObject();
		
		assertEquals(0d,dfhcommareaType.getCDouble0());
		assertEquals(1d,dfhcommareaType.getCDouble1());
		assertEquals(1234d,dfhcommareaType.getCDouble1234());
		assertEquals(345006.56779999984d,dfhcommareaType.getCDouble345006P5678());
		assertEquals(3.4028234699999995E+38,dfhcommareaType.getCDouble3P40282347Ep38());
		assertEquals(7.982006699999985E-14,dfhcommareaType.getCDouble798P20067Em16());
	}
}
