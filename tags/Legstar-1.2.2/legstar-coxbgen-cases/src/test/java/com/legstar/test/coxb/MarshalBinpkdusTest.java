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
package com.legstar.test.coxb;




import java.math.BigInteger;

import com.legstar.test.coxb.binpkdus.LsCompat;
import com.legstar.test.coxb.binpkdus.LsExtend;
import com.legstar.test.coxb.binpkdus.LsUnsignedPackedDecimal;
import com.legstar.test.coxb.binpkdus.Dfhcommarea;

import junit.framework.TestCase;

public class MarshalBinpkdusTest extends TestCase {

	private final static String SCHEMA_NAME = "binpkdus";

	public void testBinpkdus() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);
		
		LsCompat lsCompat = new LsCompat();
		lsCompat.setLsP9X1(3);
		lsCompat.setLsP9X18(123456789012345678l);
		lsCompat.setLsP9X1Null(0);
		lsCompat.setLsP9X2(12);
		lsCompat.setLsP9X7(32769);
		
		LsExtend lsExtend = new LsExtend();
		lsExtend.setLsP9X19(new BigInteger("1234567890123456789"));
		lsExtend.setLsP9X31(new BigInteger("1234567890123456789012345678901"));
		
		LsUnsignedPackedDecimal lsUnsignedPackedDecimal = new LsUnsignedPackedDecimal();
		lsUnsignedPackedDecimal.setLsCompat(lsCompat);
		lsUnsignedPackedDecimal.setLsExtend(lsExtend);
		Dfhcommarea.setLsUnsignedPackedDecimal(lsUnsignedPackedDecimal);
		
		assertEquals("0f3f012f0032769f0123456789012345678f1234567890123456789f1234567890123456789012345678901f000000000000000000000000",
				Util.marshal(SCHEMA_NAME, Dfhcommarea, 56));
	}
}
