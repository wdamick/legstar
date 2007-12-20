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
package com.legstar.host.invoke;

import java.util.Map;

import com.legstar.config.Constants;
import com.legstar.host.invoke.CicsProgram;
import com.legstar.host.invoke.CicsProgramException;

import junit.framework.TestCase;

public class CicsProgramTest extends TestCase {
	
	public void testInvalidAttributesFile() {
		try {
			@SuppressWarnings("unused")
			CicsProgram pa = new CicsProgram("tarzan.jane");
			fail("testInvalidAttributesFile failed");
		} catch (CicsProgramException e) {
			assertEquals("java.io.FileNotFoundException: tarzan.jane", e.getMessage());
		}
	}

	public void testMissingProgramName() {
		try {
			@SuppressWarnings("unused")
			CicsProgram pa = new CicsProgram("lsfileae0.properties");
			fail("testMissingProgramName failed");
		} catch (CicsProgramException e) {
			assertEquals("Program name must be specified.", e.getMessage());
		}
	}
	public void testInvalidDataLength() {
		try {
			@SuppressWarnings("unused")
			CicsProgram pa = new CicsProgram("lsfileae3.properties");
			fail("testInvalidDataLength failed");
		} catch (CicsProgramException e) {
			assertEquals("Data length cannot exceed length.", e.getMessage());
		}
	}
	
	public void testValidAttributesFileAndDefaults() {
		try {
			CicsProgram pa = new CicsProgram("lsfileae1.properties");
			assertEquals("LSFILEAE", pa.getName());
			assertEquals(0, pa.getLength());
			assertEquals(0, pa.getDataLength());
			assertEquals(null, pa.getSysID());
			assertEquals(false, pa.getSyncOnReturn());
			assertEquals(null, pa.getTransID());
		} catch (CicsProgramException e) {
			fail("testValidAttributesFile failed " + e.getMessage());
		}
	}
	public void testValidAttributesFile() {
		try {
			CicsProgram pa = new CicsProgram("lsfileae2.properties");
			assertEquals("LSFILEAE", pa.getName());
			assertEquals(735, pa.getLength());
			assertEquals(72, pa.getDataLength());
			assertEquals("ROSE", pa.getSysID());
			assertEquals(true, pa.getSyncOnReturn());
			assertEquals("CSMI", pa.getTransID());
		} catch (CicsProgramException e) {
			fail("testValidAttributesFile failed " + e.getMessage());
		}
	}
	
	public void testGetMap() {
		try {
			CicsProgram pa = new CicsProgram("lsfileae2.properties");
			Map map = pa.getProgramAttrMap();
			assertEquals("LSFILEAE", map.get(Constants.CICS_PROGRAM_NAME_KEY));
			assertEquals("735",  map.get(Constants.CICS_LENGTH_KEY));
			assertEquals("72",  map.get(Constants.CICS_DATALEN_KEY));
			assertEquals("ROSE",  map.get(Constants.CICS_SYSID_KEY));
			assertEquals("true",  map.get(Constants.CICS_SYNCONRET_KEY));
			assertEquals("CSMI",  map.get(Constants.CICS_TRANSID_KEY));
		} catch (CicsProgramException e) {
			fail("testGetMap failed " + e.getMessage());
		}
	}

	public void testChannel() {
		try {
			CicsProgram pa = new CicsProgram("container1.properties");
			assertEquals("LSFILEAC", pa.getName());
			assertEquals(0, pa.getLength());
			assertEquals(0, pa.getDataLength());
			assertEquals(null, pa.getSysID());
			assertEquals(false, pa.getSyncOnReturn());
			assertEquals(null, pa.getTransID());
			assertEquals("LSFILEAC-CHANNEL", pa.getChannel());
			assertTrue(48 == pa.getInContainers().get("QueryData"));
			assertTrue(10 == pa.getInContainers().get("QueryLimit"));
			assertTrue(794 == pa.getOutContainers().get("ReplyData"));
			assertTrue(141 == pa.getOutContainers().get("ReplyStatus"));
		} catch (CicsProgramException e) {
			fail("testChannel failed " + e.getMessage());
		}
	}

	public void testGetMapWithContainers() {
		try {
			CicsProgram pa = new CicsProgram("container1.properties");
			Map map = pa.getProgramAttrMap();
			assertEquals("LSFILEAC", map.get(Constants.CICS_PROGRAM_NAME_KEY));
			String[] outContainers = (String[]) map.get(Constants.CICS_OUT_CONTAINERS_KEY);
			assertEquals("ReplyData",  outContainers[0]);
			assertEquals("ReplyStatus",  outContainers[1]);
		} catch (CicsProgramException e) {
			fail("testGetMap failed " + e.getMessage());
		}
	}
}
