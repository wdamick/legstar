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
import com.legstar.host.invoke.ProgramAttributes;
import com.legstar.host.invoke.ProgramAttributesException;

import junit.framework.TestCase;

public class ProgramAttributesTest extends TestCase {
	
	public void testInvalidAttributesFile() {
		try {
			@SuppressWarnings("unused")
			ProgramAttributes pa = new ProgramAttributes("tarzan.jane");
			fail("testInvalidAttributesFile failed");
		} catch (ProgramAttributesException e) {
			assertEquals("java.io.FileNotFoundException: tarzan.jane", e.getMessage());
		}
	}

	public void testMissingProgramName() {
		try {
			@SuppressWarnings("unused")
			ProgramAttributes pa = new ProgramAttributes("lsfileae0.properties");
			fail("testMissingProgramName failed");
		} catch (ProgramAttributesException e) {
			assertEquals("Program name must be specified.", e.getMessage());
		}
	}
	public void testInvalidDataLength() {
		try {
			@SuppressWarnings("unused")
			ProgramAttributes pa = new ProgramAttributes("lsfileae3.properties");
			fail("testInvalidDataLength failed");
		} catch (ProgramAttributesException e) {
			assertEquals("Data length cannot exceed length.", e.getMessage());
		}
	}
	
	public void testValidAttributesFileAndDefaults() {
		try {
			ProgramAttributes pa = new ProgramAttributes("lsfileae1.properties");
			assertEquals("LSFILEAE", pa.getProgram());
			assertEquals(0, pa.getLength());
			assertEquals(0, pa.getDataLength());
			assertEquals(null, pa.getSysID());
			assertEquals(false, pa.getSyncOnReturn());
			assertEquals(null, pa.getTransID());
		} catch (ProgramAttributesException e) {
			fail("testValidAttributesFile failed " + e.getMessage());
		}
	}
	public void testValidAttributesFile() {
		try {
			ProgramAttributes pa = new ProgramAttributes("lsfileae2.properties");
			assertEquals("LSFILEAE", pa.getProgram());
			assertEquals(735, pa.getLength());
			assertEquals(72, pa.getDataLength());
			assertEquals("ROSE", pa.getSysID());
			assertEquals(true, pa.getSyncOnReturn());
			assertEquals("CSMI", pa.getTransID());
		} catch (ProgramAttributesException e) {
			fail("testValidAttributesFile failed " + e.getMessage());
		}
	}
	
	public void testGetMap() {
		try {
			ProgramAttributes pa = new ProgramAttributes("lsfileae2.properties");
			Map map = pa.getProgramAttrMap();
			assertEquals("LSFILEAE", map.get(Constants.CICS_PROGRAM_KEY));
			assertEquals("735",  map.get(Constants.CICS_LENGTH_KEY));
			assertEquals("72",  map.get(Constants.CICS_DATALEN_KEY));
			assertEquals("ROSE",  map.get(Constants.CICS_SYSID_KEY));
			assertEquals("true",  map.get(Constants.CICS_SYNCONRET_KEY));
			assertEquals("CSMI",  map.get(Constants.CICS_TRANSID_KEY));
		} catch (ProgramAttributesException e) {
			fail("testGetMap failed " + e.getMessage());
		}
	}
}
