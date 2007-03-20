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
package com.legstar.coxb.convert.simple.test;


import com.legstar.host.HostException;
import com.legstar.util.*;


import junit.framework.TestCase;

/** Program under test has a main method because it needs to be
 * invokable from ant. */
public class ByteLengthTest extends TestCase {

	public StringBuffer outContent;


	public void testLsfileae() throws HostException {
		assertEquals("79",JaxbUtil.byteLength("com.legstar.test.coxb.lsfileae","DfhcommareaType"));
	}
	
	public void testFixarsim() throws HostException {

		assertEquals("15", JaxbUtil.byteLength("com.legstar.test.coxb.fixarsim","DfhcommareaType"));
	}

	public void testFixarnum() throws HostException {

		assertEquals("78", JaxbUtil.byteLength("com.legstar.test.coxb.fixarnum","DfhcommareaType"));
	}

	public void testFixarcom() throws HostException {

		assertEquals("49", JaxbUtil.byteLength("com.legstar.test.coxb.fixarcom","DfhcommareaType"));
	}

	public void testVararcom() throws HostException {

		assertEquals("1752", JaxbUtil.byteLength("com.legstar.test.coxb.vararcom","DfhcommareaType"));
	}

	public void testBinnatsi() throws HostException {

		assertEquals("56", JaxbUtil.byteLength("com.legstar.test.coxb.binnatsi","DfhcommareaType"));
	}

	public void testBinnatus() throws HostException {

		assertEquals("56", JaxbUtil.byteLength("com.legstar.test.coxb.binnatus","DfhcommareaType"));
	}

	public void testBinnpkdus() throws HostException {

		assertEquals("44", JaxbUtil.byteLength("com.legstar.test.coxb.binpkdus","DfhcommareaType"));
	}

	public void testDplarcht() throws HostException {

		assertEquals("32025", JaxbUtil.byteLength("com.legstar.test.coxb.dplarcht","DfhcommareaType"));
	}
	

	public void testLsfilead() throws HostException {

		assertEquals("79", JaxbUtil.byteLength("com.legstar.test.coxb.lsfilead","DfhcommareaType"));
	}
	
	public void testRedbotha() throws HostException {

		assertEquals("2", JaxbUtil.byteLength("com.legstar.test.coxb.redbotha","DfhcommareaType"));
	}
	
	public void testRedinout() throws HostException {

		assertEquals("502", JaxbUtil.byteLength("com.legstar.test.coxb.redinout","DfhcommareaType"));
	}
	
	public void testRedmulti() throws HostException {

		assertEquals("206", JaxbUtil.byteLength("com.legstar.test.coxb.redmulti","DfhcommareaType"));
	}
	
	public void testRedopera() throws HostException {

		assertEquals("218", JaxbUtil.byteLength("com.legstar.test.coxb.redopera","DfhcommareaType"));
	}
	
	public void testRedsimpt() throws HostException {

		assertEquals("18", JaxbUtil.byteLength("com.legstar.test.coxb.redsimpt","DfhcommareaType"));
	}
	
	public void testTypesmix() throws HostException {

		assertEquals("176", JaxbUtil.byteLength("com.legstar.test.coxb.typesmix","DfhcommareaType"));
	}
}

