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


import java.math.BigInteger;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolMarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.host.HostData;
import com.legstar.host.HostException;
import com.legstar.test.coxb.binarcht.LsSignedNativeType;
import com.legstar.test.coxb.binarcht.LsUnsignedNativeType;

import junit.framework.TestCase;

public class MarshalBinarchtTest extends TestCase {

	public void testBinarcht() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[56];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.binarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.binarcht.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.binarcht.DfhcommareaType dfhcommareaType = objectFactory.createDfhcommareaType();
		LsSignedNativeType lsSignedNativeType = objectFactory.createLsSignedNativeType();
		lsSignedNativeType.setLsPs9X18Max(12345678901234567l);
		lsSignedNativeType.setLsPs9X18Min(-12345678901234567l);
		lsSignedNativeType.setLsPs9X4Max(new Short("32767"));
		lsSignedNativeType.setLsPs9X4Min(new Short("-32768"));
		lsSignedNativeType.setLsPs9X9Max(2147483647);
		lsSignedNativeType.setLsPs9X9Min(-123456789);
		
		dfhcommareaType.setLsSignedNative(lsSignedNativeType);
		
		LsUnsignedNativeType lsUnsignedNativeType = objectFactory.createLsUnsignedNativeType();
		lsUnsignedNativeType.setLsP9X18Max(new BigInteger("18446744073709551615"));
		lsUnsignedNativeType.setLsP9X18Min(new BigInteger("0"));
		lsUnsignedNativeType.setLsP9X4Max(65535);
		lsUnsignedNativeType.setLsP9X4Min(0);
		lsUnsignedNativeType.setLsP9X9Max(4294967295l);
		lsUnsignedNativeType.setLsP9X9Min(0);

		dfhcommareaType.setLsUnsignedNative(lsUnsignedNativeType);

		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.binarcht.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.binarcht.bind.DfhcommareaTypeBinding(objectFactory, dfhcommareaType);
		ccem.accept(mv);
		//		      <--><--><------><------><--------------><--------------><--><--><------><------><--------------><-------------->
		//		      1 2 1 2 1 2 3 4 1 2 3 4 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 1 2 1 2 3 4 1 2 3 4 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 
		//		         06553       042949672                1844674407370955-3273276-123456721474836-1234567890123451234567890123456         
		assertEquals("0000ffff00000000ffffffff0000000000000000ffffffffffffffff80007ffff8a432eb7fffffffffd423aba294b479002bdc545d6b4b87",HostData.toHexString(hostBytes));
	}
}
