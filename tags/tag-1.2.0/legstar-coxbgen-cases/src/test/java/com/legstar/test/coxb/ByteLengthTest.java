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


import junit.framework.TestCase;

public class ByteLengthTest extends TestCase {
	
	public void testFixarsim() throws Exception {
		assertEquals(15, Util.getByteLength("fixarsim"));
	}

	public void testLsfileae() throws Exception {
		assertEquals(79, Util.getByteLength("lsfileae"));
	}

	public void testRedsimpt() throws Exception {
		assertEquals(18, Util.getByteLength("redsimpt"));
	}

	public void testVararcom() throws Exception {
		assertEquals(1752, Util.getByteLength("vararcom"));
	}

	public void testBinarcht() throws Exception {
		assertEquals(56, Util.getByteLength("binarcht"));
	}

	public void testBinnatsi() throws Exception {
		assertEquals(56, Util.getByteLength("binnatsi"));
	}

	public void testDoublmix() throws Exception {
		assertEquals(48, Util.getByteLength("doublmix"));
	}

	public void testDplarcht() throws Exception {
		assertEquals(32025, Util.getByteLength("dplarcht"));
	}

	public void testFixarcom() throws Exception {
		assertEquals(49, Util.getByteLength("fixarcom"));
	}

	public void testFixarnum() throws Exception {
		assertEquals(78, Util.getByteLength("fixarnum"));
	}

	public void testRedbotha() throws Exception {
		assertEquals(2, Util.getByteLength("redbotha"));
	}
	public void testRedopera() throws Exception {
		assertEquals(218, Util.getByteLength("redopera"));
	}
	public void testTypesmix() throws Exception {
		assertEquals(176, Util.getByteLength("typesmix"));
	}

	public void testAlltypes() throws Exception {
		assertEquals(267, Util.getByteLength("alltypes"));
	}
	
	public void testRedinout() throws Exception {
		assertEquals(502, Util.getByteLength("redinout"));
	}
}
