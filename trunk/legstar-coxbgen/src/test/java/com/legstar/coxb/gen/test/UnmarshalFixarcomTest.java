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
import com.legstar.coxb.CobolUnmarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;

public class UnmarshalFixarcomTest extends TestCase {

	public void testFixarcom() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		//		              <------------><------------><------------><------------><------------><------------><------------>
		//		              1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 
		//		              A B J A 0    0A B J A 1    7A B J A 0   14A B J A 3   21A B J A 4   28A B J A 5   35A B J A 6   42             
		String hexString   = "c1c2d1c1f00000c1c2d1c1f10007c1c2d1c1f2000ec1c2d1c1f30015c1c2d1c1f4001cc1c2d1c1f50023c1c2d1c1f6002a";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.fixarcom.ObjectFactory objectFactory = new com.legstar.test.coxb.fixarcom.ObjectFactory();

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.fixarcom.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.fixarcom.bind.DfhcommareaTypeBinding(objectFactory);
		ccem.accept(uv);
		com.legstar.test.coxb.fixarcom.DfhcommareaType dfhcommareaType = ccem.getJaxbObject();
		
		for(int i=0;i< 7;i++) {
			com.legstar.test.coxb.fixarcom.CArrayType item = dfhcommareaType.getCArray().get(i);
			assertEquals("ABJA" + Integer.toString(i),item.getCItem1());
			assertEquals(Short.parseShort(Integer.toString(7 * i)),item.getCItem2());
		}
	}
}
