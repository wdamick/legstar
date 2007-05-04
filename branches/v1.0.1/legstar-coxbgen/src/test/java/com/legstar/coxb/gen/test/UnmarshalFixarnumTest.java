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

public class UnmarshalFixarnumTest extends TestCase {

	public void testFixarnum() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		//		              <--C-ARRAY-PD-COMP-3-- ><-------C-ARRAY-ZD-DISPLAY---------><--C-ARRAY-ZI-DISPLAY--><---C-ARRAY-BI-COMP----><------------C-ARRAY-NI-COMP-5----------------->
		//		              <------><------><------><----------><----------><----------><------><------><------><------><------><------><--------------><--------------><-------------->
		//		              1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 5 6 1 2 3 4 5 6 1 2 3 4 5 6 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 
		//		              1653423+0000150+0018400+5 3 4 2 3 6 0 4 5 0 0 7 0 0 1 9 5 0 9 9 9 8 0 0 0 0 0 1 7 89998999980067676736789013
		String hexString   = "1653423f0000150f0018400ff5f3f4f2f3f6f0f4f5f0f0f7f0f0f1f9f5f0f9f9f9f8f0f0f0f0f0f1f7f83b99435e000a539f02315b1501b69b4ba630f34e00000001936299fe0000000002315bc0";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.fixarnum.ObjectFactory objectFactory = new com.legstar.test.coxb.fixarnum.ObjectFactory();

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.fixarnum.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.fixarnum.bind.DfhcommareaTypeBinding(objectFactory);
		ccem.accept(uv);
		com.legstar.test.coxb.fixarnum.DfhcommareaType dfhcommareaType = ccem.getJaxbObject();
		
		assertEquals("16534.23",dfhcommareaType.getCArrayPd().get(0).toString());
		assertEquals("1.50",dfhcommareaType.getCArrayPd().get(1).toString());
		assertEquals("184.00",dfhcommareaType.getCArrayPd().get(2).toString());
		
		assertEquals("534.236",dfhcommareaType.getCArrayZd().get(0).toString());
		assertEquals("45.007",dfhcommareaType.getCArrayZd().get(1).toString());
		assertEquals("1.950",dfhcommareaType.getCArrayZd().get(2).toString());
		
		assertEquals("9998",dfhcommareaType.getCArrayZi().get(0).toString());
		assertEquals("0",dfhcommareaType.getCArrayZi().get(1).toString());
		assertEquals("178",dfhcommareaType.getCArrayZi().get(2).toString());

		assertEquals("999899998",dfhcommareaType.getCArrayBi().get(0).toString());
		assertEquals("676767",dfhcommareaType.getCArrayBi().get(1).toString());
		assertEquals("36789013",dfhcommareaType.getCArrayBi().get(2).toString());

		assertEquals("123456789012345678",dfhcommareaType.getCArrayNi().get(0).toString());
		assertEquals("6767679998",dfhcommareaType.getCArrayNi().get(1).toString());
		assertEquals("36789184",dfhcommareaType.getCArrayNi().get(2).toString());
	}
}
