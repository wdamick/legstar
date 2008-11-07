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

import com.legstar.test.coxb.binarcht.LsSignedNative;
import com.legstar.test.coxb.binarcht.LsUnsignedNative;
import com.legstar.test.coxb.binarcht.Dfhcommarea;

import junit.framework.TestCase;

public class MarshalBinarchtTest extends TestCase {
	
	private final static String SCHEMA_NAME = "binarcht";

	public void testBinarcht() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);
		LsSignedNative lsSignedNative = new LsSignedNative();
		lsSignedNative.setLsPs9X18Max(12345678901234567l);
		lsSignedNative.setLsPs9X18Min(-12345678901234567l);
		lsSignedNative.setLsPs9X4Max(new Short("32767"));
		lsSignedNative.setLsPs9X4Min(new Short("-32768"));
		lsSignedNative.setLsPs9X9Max(2147483647);
		lsSignedNative.setLsPs9X9Min(-123456789);
		
		Dfhcommarea.setLsSignedNative(lsSignedNative);
		
		LsUnsignedNative lsUnsignedNative = new LsUnsignedNative();
		lsUnsignedNative.setLsP9X18Max(new BigInteger("18446744073709551615"));
		lsUnsignedNative.setLsP9X18Min(new BigInteger("0"));
		lsUnsignedNative.setLsP9X4Max(65535);
		lsUnsignedNative.setLsP9X4Min(0);
		lsUnsignedNative.setLsP9X9Max(4294967295l);
		lsUnsignedNative.setLsP9X9Min(0);

		Dfhcommarea.setLsUnsignedNative(lsUnsignedNative);

		//		      <--><--><------><------><--------------><--------------><--><--><------><------><--------------><-------------->
		//		      1 2 1 2 1 2 3 4 1 2 3 4 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 1 2 1 2 3 4 1 2 3 4 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 
		//		         06553       042949672                1844674407370955-3273276-123456721474836-1234567890123451234567890123456         
		assertEquals("0000ffff00000000ffffffff0000000000000000ffffffffffffffff80007ffff8a432eb7fffffffffd423aba294b479002bdc545d6b4b87",
				Util.marshal(SCHEMA_NAME, Dfhcommarea, 56));
	}
}
