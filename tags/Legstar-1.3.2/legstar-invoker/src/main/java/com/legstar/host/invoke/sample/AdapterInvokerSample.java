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
package com.legstar.host.invoke.sample;

import java.util.Arrays;

import com.legstar.host.invoke.HostInvoker;
import com.legstar.host.invoke.HostInvokerFactory;
import com.legstar.host.invoke.HostProgramProperties;
import com.legstar.messaging.LegStarAddress;

/**
 * A sample program using Mainframe Invoker to invoke a CICS program.
 *
 */
public class AdapterInvokerSample {
    
    /** A valid invoker configuration file. */
    public static final String CONFIG_FILE = "legstar-invoker-config.xml";
    
    /** Target CICS program attributes. */
    public static final String PROGRAM_PROPERTIES = "lsfileae.properties";

    /**
     * Main entry point.
     * @param args takes no arguments
     * @throws Exception if something goes wrong
     */
    public static void main(final String[] args) throws Exception {
        AdapterInvokerSample invoker = new AdapterInvokerSample();
        invoker.invoke();
    }
    
    /**
     * Run the remote program using the sample configuration.
     * <p/>
     * This will perform the following:
     * <ul>
     * <li>Create a mainframe invoker using the configuration files</li>
     * <li>Pass on raw data corresponding to the EBCDIC content of the expected
     * commarea</li>
     * <li>Print out the raw reply received which corresponds to the EBCDIC
     * content of the output commarea</li>
     * </ul>
     * @throws Exception if something goes wrong
     */
    public void invoke() throws Exception {
        LegStarAddress address = new LegStarAddress("TheMainframe");
        HostInvoker invoker = HostInvokerFactory.createHostInvoker(
                CONFIG_FILE, address, new HostProgramProperties(PROGRAM_PROPERTIES));
        byte[] replyBytes = invoker.invoke("LsfileaeSampleTest",
                new byte[] {(byte) 0xf0, (byte) 0xf0, (byte) 0xf0,
                (byte) 0xf1, (byte) 0xf0, (byte) 0xf0});
        System.out.println(Arrays.toString(replyBytes));
        System.out.println(invoker.getHostProgram().getName()
                + " invoked successfully");
    }
}
