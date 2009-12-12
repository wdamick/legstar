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
package com.legstar.host.invoke;

import com.legstar.host.invoke.model.HostContainer;
import com.legstar.host.invoke.model.HostProgramException;

import junit.framework.TestCase;

/**
 * Test the CicsProgram structure.
 *
 */
public class HostProgramPropertiesTest extends TestCase {

    /**
     * Test with invalid program properties file.
     */
    public void testInvalidAttributesFile() {
        try {
            new HostProgramProperties("tarzan.jane");
            fail("testInvalidAttributesFile failed");
        } catch (HostProgramException e) {
            assertEquals("java.io.FileNotFoundException: tarzan.jane", e.getMessage());
        }
    }

    /**
     * Test with a no program names in properties file.
     */
    public void testMissingProgramName() {
        try {
            new HostProgramProperties("lsfileae0.properties");
            fail("testMissingProgramName failed");
        } catch (HostProgramException e) {
            assertEquals("Program name must be specified.", e.getMessage());
        }
    }
    
    /**
     * Test with a data length that is too large.
     */
    public void testInvalidDataLength() {
        try {
            new HostProgramProperties("lsfileae3.properties");
            fail("testInvalidDataLength failed");
        } catch (HostProgramException e) {
            assertEquals("Data length cannot exceed length.", e.getMessage());
        }
    }

    /**
     * Check with a valid properties file partially filled.
     */
    public void testValidAttributesFileAndDefaults() {
        try {
            HostProgramProperties pa = new HostProgramProperties("lsfileae1.properties");
            assertEquals("LSFILEAE", pa.getName());
            assertEquals(0, pa.getLength());
            assertEquals(0, pa.getDataLength());
            assertEquals(null, pa.getSysID());
            assertTrue(null == pa.getSyncOnReturn());
            assertEquals(null, pa.getTransID());
        } catch (HostProgramException e) {
            fail("testValidAttributesFile failed " + e.getMessage());
        }
    }

    /**
     * Check with a valid properties file.
     */
    public void testValidAttributesFile() {
        try {
            HostProgramProperties pa = new HostProgramProperties("lsfileae2.properties");
            assertEquals("LSFILEAE", pa.getName());
            assertEquals(735, pa.getLength());
            assertEquals(72, pa.getDataLength());
            assertEquals("ROSE", pa.getSysID());
            assertEquals(true, pa.getSyncOnReturn().booleanValue());
            assertEquals("CSMI", pa.getTransID());
        } catch (HostProgramException e) {
            fail("testValidAttributesFile failed " + e.getMessage());
        }
    }

    /**
     * Test a Channel/Container case.
     */
    public void testChannel() {
        try {
            HostProgramProperties pa = new HostProgramProperties("container1.properties");
            assertEquals("LSFILEAC", pa.getName());
            assertEquals(0, pa.getLength());
            assertEquals(0, pa.getDataLength());
            assertEquals(null, pa.getSysID());
            assertTrue(null == pa.getSyncOnReturn());
            assertEquals(null, pa.getTransID());
            assertEquals("LSFILEAC-CHANNEL", pa.getChannel());
            for (HostContainer container : pa.getInContainers()) {
                if (container.getName().equals("QueryData")) {
                    assertEquals(48, container.getLength());
                } else if (container.getName().equals("QueryLimit")) {
                    assertEquals(10, container.getLength());
                } else {
                    fail();
                }
                
            }
            for (HostContainer container : pa.getOutContainers()) {
                if (container.getName().equals("ReplyData")) {
                    assertEquals(794, container.getLength());
                } else if (container.getName().equals("ReplyStatus")) {
                    assertEquals(141, container.getLength());
                } else {
                    fail();
                }
                
            }
        } catch (HostProgramException e) {
            fail("testChannel failed " + e.getMessage());
        }
    }

}
