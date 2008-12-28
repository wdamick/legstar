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
package com.legstar.host.invoke;

import java.util.Map;

import com.legstar.config.Constants;

import junit.framework.TestCase;

/**
 * Test the CicsProgram structure.
 *
 */
public class CicsProgramTest extends TestCase {

    /**
     * Test with invalid program properties file.
     */
    public void testInvalidAttributesFile() {
        try {
            new CicsProgram("tarzan.jane");
            fail("testInvalidAttributesFile failed");
        } catch (CicsProgramException e) {
            assertEquals("java.io.FileNotFoundException: tarzan.jane", e.getMessage());
        }
    }

    /**
     * Test with a no program names in properties file.
     */
    public void testMissingProgramName() {
        try {
            new CicsProgram("lsfileae0.properties");
            fail("testMissingProgramName failed");
        } catch (CicsProgramException e) {
            assertEquals("Program name must be specified.", e.getMessage());
        }
    }
    
    /**
     * Test with a data length that is too large.
     */
    public void testInvalidDataLength() {
        try {
            new CicsProgram("lsfileae3.properties");
            fail("testInvalidDataLength failed");
        } catch (CicsProgramException e) {
            assertEquals("Data length cannot exceed length.", e.getMessage());
        }
    }

    /**
     * Check with a valid properties file partially filled.
     */
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

    /**
     * Check with a valid properties file.
     */
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

    /**Check getting the internal map.
     * 
     */
    public void testGetMap() {
        try {
            CicsProgram pa = new CicsProgram("lsfileae2.properties");
            Map < String, Object> map = pa.getProgramAttrMap();
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

    /**
     * Test a Channel/Container case.
     */
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

    /**
     * Test a map with Channel/Container case.
     */
    public void testGetMapWithContainers() {
        try {
            CicsProgram pa = new CicsProgram("container1.properties");
            Map < String, Object> map = pa.getProgramAttrMap();
            assertEquals("LSFILEAC", map.get(Constants.CICS_PROGRAM_NAME_KEY));
            String[] outContainers = (String[]) map.get(Constants.CICS_OUT_CONTAINERS_KEY);
            assertEquals("ReplyData",  outContainers[0]);
            assertEquals("ReplyStatus",  outContainers[1]);
        } catch (CicsProgramException e) {
            fail("testGetMap failed " + e.getMessage());
        }
    }
}
