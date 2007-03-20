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
package com.legstar.coxb.gen.test;

import junit.framework.TestCase;

import com.legstar.host.HostException;

public class ByteLengthTest extends TestCase {
	
	public void testFixarsim() throws HostException {
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.fixarsim.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.fixarsim.bind.DfhcommareaTypeBinding();
		assertEquals(15, ccem.getByteLength());
	}

	public void testLsfileae() throws HostException {
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.lsfileae.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.lsfileae.bind.DfhcommareaTypeBinding();
		assertEquals(79, ccem.getByteLength());
	}

	public void testRedsimpt() throws HostException {
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.redsimpt.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.redsimpt.bind.DfhcommareaTypeBinding();
		assertEquals(18, ccem.getByteLength());
	}

	public void testVararcom() throws HostException {
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.vararcom.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.vararcom.bind.DfhcommareaTypeBinding();
		assertEquals(1752, ccem.getByteLength());
	}

	public void testBinarcht() throws HostException {
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.binarcht.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.binarcht.bind.DfhcommareaTypeBinding();
		assertEquals(56, ccem.getByteLength());
	}

	public void testBinnatsi() throws HostException {
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.binnatsi.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.binnatsi.bind.DfhcommareaTypeBinding();
		assertEquals(56, ccem.getByteLength());
	}

	public void testDoublmix() throws HostException {
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.doublmix.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.doublmix.bind.DfhcommareaTypeBinding();
		assertEquals(48, ccem.getByteLength());
	}

	public void testDplarcht() throws HostException {
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.dplarcht.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.dplarcht.bind.DfhcommareaTypeBinding();
		assertEquals(32025, ccem.getByteLength());
	}

	public void testFixarcom() throws HostException {
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.fixarcom.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.fixarcom.bind.DfhcommareaTypeBinding();
		assertEquals(49, ccem.getByteLength());
	}

	public void testFixarnum() throws HostException {
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.fixarnum.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.fixarnum.bind.DfhcommareaTypeBinding();
		assertEquals(78, ccem.getByteLength());
	}

	public void testRedbotha() throws HostException {
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.redbotha.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.redbotha.bind.DfhcommareaTypeBinding();
		assertEquals(2, ccem.getByteLength());
	}
	public void testRedopera() throws HostException {
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.redopera.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.redopera.bind.DfhcommareaTypeBinding();
		assertEquals(218, ccem.getByteLength());
	}
	public void testTypesmix() throws HostException {
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.typesmix.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.typesmix.bind.DfhcommareaTypeBinding();
		assertEquals(176, ccem.getByteLength());
	}

	public void testAlltypes() throws HostException {
		// Traverse the object structure, visiting each node with the visitor
		com.legstar.test.coxb.alltypes.bind.DfhcommareaTypeBinding ccem = new com.legstar.test.coxb.alltypes.bind.DfhcommareaTypeBinding();
		assertEquals(267, ccem.getByteLength());
	}
}
