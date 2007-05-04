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



import java.math.BigInteger;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolMarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.host.HostData;
import com.legstar.host.HostException;
import com.legstar.test.coxb.binpkdus.LsCompatType;
import com.legstar.test.coxb.binpkdus.LsExtendType;
import com.legstar.test.coxb.binpkdus.LsUnsignedPackedDecimalType;

import junit.framework.TestCase;

public class MarshalBinpkdusTest extends TestCase {

	public void testBinpkdus() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[56];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.binpkdus.ObjectFactory objectFactory = new com.legstar.test.coxb.binpkdus.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.binpkdus.DfhcommareaType dfhcommareaType = objectFactory.createDfhcommareaType();
		
		LsCompatType lsCompatType = objectFactory.createLsCompatType();
		lsCompatType.setLsP9X1(3);
		lsCompatType.setLsP9X18(123456789012345678l);
		lsCompatType.setLsP9X1Null(0);
		lsCompatType.setLsP9X2(12);
		lsCompatType.setLsP9X7(32769);
		
		LsExtendType lsExtendType = objectFactory.createLsExtendType();
		lsExtendType.setLsP9X19(new BigInteger("1234567890123456789"));
		lsExtendType.setLsP9X31(new BigInteger("1234567890123456789012345678901"));
		
		LsUnsignedPackedDecimalType lsUnsignedPackedDecimalType = objectFactory.createLsUnsignedPackedDecimalType();
		lsUnsignedPackedDecimalType.setLsCompat(lsCompatType);
		lsUnsignedPackedDecimalType.setLsExtend(lsExtendType);
		dfhcommareaType.setLsUnsignedPackedDecimal(lsUnsignedPackedDecimalType);
		
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.binpkdus.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.binpkdus.bind.DfhcommareaTypeBinding(objectFactory, dfhcommareaType);
		ccem.accept(mv);
		assertEquals("0f3f012f0032769f0123456789012345678f1234567890123456789f1234567890123456789012345678901f000000000000000000000000",HostData.toHexString(hostBytes));
	}
}
