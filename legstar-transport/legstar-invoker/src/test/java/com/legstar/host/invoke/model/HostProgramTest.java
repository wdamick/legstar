/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.host.invoke.model;

import junit.framework.TestCase;

/**
 * Test the HostProgram class.
 * 
 */
public class HostProgramTest extends TestCase {

    /**
     * Check that class instantiates and serializes to string for commarea
     * content.
     */
    public void testInstentiationCommarea() {
        HostProgram hostProgram = new HostProgram();
        assertEquals("{\"CICSProgramName\":null," + "\"CICSLength\":0,"
                + "\"CICSDataLength\":0}", hostProgram.toString());
        hostProgram.setName("krakatoa");
        assertEquals("{\"CICSProgramName\":\"krakatoa\"," + "\"CICSLength\":0,"
                + "\"CICSDataLength\":0}", hostProgram.toString());
        hostProgram.setMaxDataLength(18);
        assertEquals("{\"CICSProgramName\":\"krakatoa\","
                + "\"CICSLength\":18," + "\"CICSDataLength\":0}",
                hostProgram.toString());
        hostProgram.setDataLength(4);
        assertEquals("{\"CICSProgramName\":\"krakatoa\","
                + "\"CICSLength\":18," + "\"CICSDataLength\":4}",
                hostProgram.toString());
    }

    /**
     * Check that class instantiates and serializes to string for containers
     * content.
     */
    public void testInstentiationContainers() {
        HostProgram hostProgram = new HostProgram();
        hostProgram.setName("krakatoa");
        hostProgram.setChannelName("krakatoaChannel");
        assertEquals("{\"CICSProgramName\":\"krakatoa\","
                + "\"CICSChannel\":\"krakatoaChannel\"}",
                hostProgram.toString());
        HostContainer hostContainer1 = new HostContainer();
        hostContainer1.setName("buzz");
        hostContainer1.setLength(256);
        hostProgram.getInputContainers().add(hostContainer1);
        assertEquals(
                "{\"CICSProgramName\":\"krakatoa\","
                        + "\"CICSChannel\":\"krakatoaChannel\","
                        + "\"CICSInContainers\":["
                        + "{\"containerLength\":256,\"containerName\":\"buzz\"}"
                        + "]}", hostProgram.toString());
        HostContainer hostContainer2 = new HostContainer();
        hostContainer2.setName("bing");
        hostContainer2.setLength(23);
        hostProgram.getInputContainers().add(hostContainer2);
        assertEquals("{\"CICSProgramName\":\"krakatoa\","
                + "\"CICSChannel\":\"krakatoaChannel\","
                + "\"CICSInContainers\":["
                + "{\"containerLength\":256,\"containerName\":\"buzz\"},"
                + "{\"containerLength\":23,\"containerName\":\"bing\"}" + "]}",
                hostProgram.toString());
        HostContainer hostContainer3 = new HostContainer();
        hostContainer3.setName("fare");
        hostContainer3.setLength(78);
        hostProgram.getOutputContainers().add(hostContainer3);
        assertEquals("{\"CICSProgramName\":\"krakatoa\","
                + "\"CICSChannel\":\"krakatoaChannel\","
                + "\"CICSInContainers\":["
                + "{\"containerLength\":256,\"containerName\":\"buzz\"},"
                + "{\"containerLength\":23,\"containerName\":\"bing\"}" + "],"
                + "\"CICSOutContainers\":["
                + "{\"containerLength\":78,\"containerName\":\"fare\"}" + "]}",
                hostProgram.toString());
        HostContainer hostContainer4 = new HostContainer();
        hostContainer4.setName("niente");
        hostContainer4.setLength(9);
        hostProgram.getOutputContainers().add(hostContainer4);
        assertEquals(
                "{\"CICSProgramName\":\"krakatoa\","
                        + "\"CICSChannel\":\"krakatoaChannel\","
                        + "\"CICSInContainers\":["
                        + "{\"containerLength\":256,\"containerName\":\"buzz\"},"
                        + "{\"containerLength\":23,\"containerName\":\"bing\"}"
                        + "],"
                        + "\"CICSOutContainers\":["
                        + "{\"containerLength\":78,\"containerName\":\"fare\"},"
                        + "{\"containerLength\":9,\"containerName\":\"niente\"}"
                        + "]}", hostProgram.toString());
    }

    /**
     * Check that class instantiates and serializes to string for extra content.
     */
    public void testInstentiationExtra() {
        HostProgram hostProgram = new HostProgram();
        hostProgram.setName("krakatoa");
        hostProgram.setMaxDataLength(18);
        hostProgram.setDataLength(4);
        hostProgram.setSysID("MYsysID");
        hostProgram.setSyncOnReturn(true);
        hostProgram.setTransID("MYtransID");
        assertEquals("{\"CICSProgramName\":\"krakatoa\","
                + "\"CICSLength\":18," + "\"CICSDataLength\":4,"
                + "\"CICSSysID\":\"MYsysID\"," + "\"CICSSyncOnReturn\":true,"
                + "\"CICSTransID\":\"MYtransID\"}", hostProgram.toString());
    }

    /**
     * Check the host serialization of attributes for commarea-driven programs.
     */
    public void testToHostCommarea() {
        try {
            HostProgram hostProgram = new HostProgram();
            hostProgram.setName("krakatoa");
            hostProgram.setMaxDataLength(18);
            hostProgram.setDataLength(4);
            hostProgram.setSysID("MYsysID");
            hostProgram.setSyncOnReturn(true);
            hostProgram.setTransID("MYtransID");
            assertEquals("{\"CICSProgramName\":\"krakatoa\","
                    + "\"CICSLength\":\"18\"," + "\"CICSDataLength\":\"4\","
                    + "\"CICSSysID\":\"MYsysID\","
                    + "\"CICSSyncOnReturn\":\"true\","
                    + "\"CICSTransID\":\"MYtransID\"}",
                    hostProgram.toJSONHost());
        } catch (HostProgramException e) {
            fail(e.toString());
        }
    }

    /**
     * Check the host serialization of attributes for containers-driven
     * programs.
     */
    public void testToHostContainers() {
        try {
            HostProgram hostProgram = new HostProgram();
            hostProgram.setName("krakatoa");
            hostProgram.setChannelName("krakatoaChannel");
            HostContainer hostContainer3 = new HostContainer();
            hostContainer3.setName("fare");
            hostContainer3.setLength(78);
            hostProgram.getOutputContainers().add(hostContainer3);
            HostContainer hostContainer4 = new HostContainer();
            hostContainer4.setName("niente");
            hostContainer4.setLength(9);
            hostProgram.getOutputContainers().add(hostContainer4);
            assertEquals("{\"CICSProgramName\":\"krakatoa\","
                    + "\"CICSChannel\":\"krakatoaChannel\","
                    + "\"CICSOutContainers\":[\"fare\",\"niente\"]}",
                    hostProgram.toJSONHost());
        } catch (HostProgramException e) {
            fail(e.toString());
        }
    }
}
