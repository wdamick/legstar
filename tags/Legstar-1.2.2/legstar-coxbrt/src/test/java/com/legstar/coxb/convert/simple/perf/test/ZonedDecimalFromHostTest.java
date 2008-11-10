/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.convert.simple.perf.test;

import java.math.BigDecimal;

import com.legstar.coxb.convert.simple.CobolZonedDecimalSimpleConverter;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

public class ZonedDecimalFromHostTest extends TestCase {
	
	private static int ITERATIONS = 100;
	public void testToHost() throws HostException {
		// Create a host buffer with host input
		byte[] hostBytes = HostData.toByteArray("f4f5f6f7f9f0f0f0f6f7f5");
   	
		for (int i = 0; i < ITERATIONS; i++) {
			BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter.fromHostSingle(11, 11, 5, false,false,false,hostBytes, 0, "IBM01140");
			assertEquals("456790.00675", javaDecimal.toString());
		}
	}

}
