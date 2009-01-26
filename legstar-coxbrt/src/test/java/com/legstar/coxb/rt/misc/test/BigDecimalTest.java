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
package com.legstar.coxb.rt.misc.test;

import junit.framework.TestCase;

public class BigDecimalTest extends TestCase {
	/* The new precision in JDK 1.5 does not always include the scale.
	 * in the case 0.00 (or .00), the scale is 2 while the precision is 1! */
		
	public void testBD1() {
		java.math.BigDecimal javaDecimal = new java.math.BigDecimal("345.25");
		assertEquals(5, javaDecimal.precision());
		assertEquals(2, javaDecimal.scale());
	}

	public void testBD2() {
		java.math.BigDecimal javaDecimal = new java.math.BigDecimal("345.00");
		assertEquals(5, javaDecimal.precision());
		assertEquals(2, javaDecimal.scale());
	}

	public void testBD3() {
		java.math.BigDecimal javaDecimal = new java.math.BigDecimal("345");
		assertEquals(3, javaDecimal.precision());
		assertEquals(0, javaDecimal.scale());
	}

	public void testBD4() {
		java.math.BigDecimal javaDecimal = new java.math.BigDecimal("0");
		assertEquals(1, javaDecimal.precision());
		assertEquals(0, javaDecimal.scale());
	}

	public void testBD5() {
		java.math.BigDecimal javaDecimal = new java.math.BigDecimal("0.00");
		assertEquals(1, javaDecimal.precision());
		assertEquals(2, javaDecimal.scale());
	}

	public void testBD6() {
		java.math.BigDecimal javaDecimal = new java.math.BigDecimal(".00");
		assertEquals(1, javaDecimal.precision());
		assertEquals(2, javaDecimal.scale());
	}

}
