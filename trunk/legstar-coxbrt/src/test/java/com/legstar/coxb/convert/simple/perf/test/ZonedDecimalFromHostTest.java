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
package com.legstar.coxb.convert.simple.perf.test;

import java.math.BigDecimal;

import com.legstar.coxb.convert.simple.CobolZonedDecimalSimpleConverter;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;

public class ZonedDecimalFromHostTest extends TestCase {
	
	private static int ITERATIONS = 100;
	public void testToHost() throws HostException {
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f4f5f6f7f9f0f0f0f6f7f5");
   	
		for (int i = 0; i < ITERATIONS; i++) {
			BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(11, 11, 5, false,false,false,hostBytes, 0);
			assertEquals("456790.00675", javaDecimal.toString());
		}
	}

}
