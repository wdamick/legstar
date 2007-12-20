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
