/*******************************************************************************
 * Copyright (c) 2009 LegSem.
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
