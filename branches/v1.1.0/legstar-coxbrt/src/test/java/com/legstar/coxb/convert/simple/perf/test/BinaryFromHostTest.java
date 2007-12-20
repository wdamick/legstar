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
package com.legstar.coxb.convert.simple.perf.test;

import java.math.BigDecimal;

import com.legstar.coxb.convert.simple.CobolBinarySimpleConverter;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

/**
 * BinaryFromHostTest.
 *
 */
public class BinaryFromHostTest extends TestCase {
	
	/** Test times.	 */
	private static final int ITERATIONS = 100;
	
	/**
	 * From Java to Host.
	 * @throws HostException if anything goes wrong
	 */
	public final void testToHost() throws HostException {
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("0000000aa2ae3c63");
   	
		for (int i = 0; i < ITERATIONS; i++) {
			BigDecimal javaDecimal =
				CobolBinarySimpleConverter.fromHostSingle(
						8, false, 11, 5, hostBytes, 0);
			assertEquals("456790.00675", javaDecimal.toString());
		}
	}

}
