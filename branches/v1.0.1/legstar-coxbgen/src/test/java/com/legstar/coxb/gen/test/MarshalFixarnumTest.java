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


import java.math.BigDecimal;
import java.math.BigInteger;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolMarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;

public class MarshalFixarnumTest extends TestCase {

	public void testFixarnum() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[78];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.fixarnum.ObjectFactory objectFactory = new com.legstar.test.coxb.fixarnum.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.fixarnum.DfhcommareaType dfhcommareaType = objectFactory.createDfhcommareaType();

		dfhcommareaType.getCArrayPd().add(new BigDecimal("16534.23"));
		dfhcommareaType.getCArrayPd().add(new BigDecimal("1.5"));
		dfhcommareaType.getCArrayPd().add(new BigDecimal("184"));
		
		dfhcommareaType.getCArrayZd().add(new BigDecimal("534.236"));
		dfhcommareaType.getCArrayZd().add(new BigDecimal("45.007"));
		dfhcommareaType.getCArrayZd().add(new BigDecimal("1.95"));
		
		dfhcommareaType.getCArrayZi().add(new Integer("9998"));
		dfhcommareaType.getCArrayZi().add(new Integer("0"));
		dfhcommareaType.getCArrayZi().add(new Integer("178"));

		dfhcommareaType.getCArrayBi().add(new Long("999899998"));
		dfhcommareaType.getCArrayBi().add(new Long("676767"));
		dfhcommareaType.getCArrayBi().add(new Long("36789013"));

		dfhcommareaType.getCArrayNi().add(new BigInteger("123456789012345678"));
		dfhcommareaType.getCArrayNi().add(new BigInteger("6767679998"));
		dfhcommareaType.getCArrayNi().add(new BigInteger("36789184"));


		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.fixarnum.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.fixarnum.bind.DfhcommareaTypeBinding(objectFactory, dfhcommareaType);
		ccem.accept(mv);
		//		      <--C-ARRAY-PD-COMP-3-- ><-------C-ARRAY-ZD-DISPLAY---------><--C-ARRAY-ZI-DISPLAY--><---C-ARRAY-BI-COMP----><------------C-ARRAY-NI-COMP-5----------------->
		//		      <------><------><------><----------><----------><----------><------><------><------><------><------><------><--------------><--------------><-------------->
		//		      1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 5 6 1 2 3 4 5 6 1 2 3 4 5 6 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 
		//		      1653423+0000150+0018400+5 3 4 2 3 6 0 4 5 0 0 7 0 0 1 9 5 0 9 9 9 8 0 0 0 0 0 1 7 89998999980067676736789013
		assertEquals("1653423f0000150f0018400ff5f3f4f2f3f6f0f4f5f0f0f7f0f0f1f9f5f0f9f9f9f8f0f0f0f0f0f1f7f83b99435e000a539f02315b1501b69b4ba630f34e00000001936299fe0000000002315bc0",HostData.toHexString(hostBytes));
	}
}
