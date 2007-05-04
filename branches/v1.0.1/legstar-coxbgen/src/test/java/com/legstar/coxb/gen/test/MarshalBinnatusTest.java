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
import com.legstar.test.coxb.binnatus.LsUnsignedNativeType;
import com.legstar.test.coxb.binnatus.LsDoublewordsType;
import com.legstar.test.coxb.binnatus.LsFullwordsType;
import com.legstar.test.coxb.binnatus.LsHalfwordsType;

import junit.framework.TestCase;

public class MarshalBinnatusTest extends TestCase {

	public void testBinnatus() throws HostException {

		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		// Create a concrete visitor
		byte[] hostBytes = new byte[56];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);

		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.binnatus.ObjectFactory objectFactory = new com.legstar.test.coxb.binnatus.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.binnatus.DfhcommareaType dfhcommareaType = objectFactory.createDfhcommareaType();
		
		LsUnsignedNativeType lsUnsignedNativeType = objectFactory.createLsUnsignedNativeType(); 
		
		LsHalfwordsType lsHalfwordsType = objectFactory.createLsHalfwordsType();
		lsHalfwordsType.setLsP9X4High(32769);
		lsHalfwordsType.setLsP9X4Low(127);
		lsHalfwordsType.setLsP9X4Max(65535);
		lsHalfwordsType.setLsP9X4Min(0);
		
		LsFullwordsType lsFullwordsType = objectFactory.createLsFullwordsType();
		lsFullwordsType.setLsP9X9High(2147483649l);
		lsFullwordsType.setLsP9X9Low(65534);
		lsFullwordsType.setLsP9X9Max(4294967295l);
		lsFullwordsType.setLsP9X9Min(0);
		
		LsDoublewordsType lsDoublewordsType = objectFactory.createLsDoublewordsType();
		lsDoublewordsType.setLsP9X18High(new BigInteger("18446744069414584318"));
		lsDoublewordsType.setLsP9X18Low(new BigInteger("4294967294"));
		lsDoublewordsType.setLsP9X18Max(new BigInteger("18446744073709551615"));
		lsDoublewordsType.setLsP9X18Min(new BigInteger("0"));
		
		lsUnsignedNativeType.setLsHalfwords(lsHalfwordsType);
		lsUnsignedNativeType.setLsFullwords(lsFullwordsType);
		lsUnsignedNativeType.setLsDoublewords(lsDoublewordsType);
		
		dfhcommareaType.setLsUnsignedNative(lsUnsignedNativeType);
		
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.binnatus.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.binnatus.bind.DfhcommareaTypeBinding(objectFactory, dfhcommareaType);
		ccem.accept(mv);
		assertEquals("0000007f8001ffff000000000000fffe80000001ffffffff000000000000000000000000fffffffefffffffefffffffeffffffffffffffff",HostData.toHexString(hostBytes));
	}
}
