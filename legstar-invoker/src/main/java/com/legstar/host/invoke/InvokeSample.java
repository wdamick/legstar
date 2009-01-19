package com.legstar.host.invoke;

import java.util.Arrays;

import com.legstar.messaging.LegStarAddress;

/**
 * A sample program using Mainframe Invoker to invoke a CICS program.
 *
 */
public class InvokeSample {
    
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
        InvokeSample cicsSocketSample = new InvokeSample();
        cicsSocketSample.testInvoke();
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
    public void testInvoke() throws Exception {
        LegStarAddress address = new LegStarAddress("TheMainframe");
        HostInvoker invoker = HostInvokerFactory.createHostInvoker(
                CONFIG_FILE, address, PROGRAM_PROPERTIES);
        byte[] replyBytes = invoker.invoke("LsfileaeSampleTest",
                new byte[] {(byte) 0xf0, (byte) 0xf0, (byte) 0xf0,
                (byte) 0xf1, (byte) 0xf0, (byte) 0xf0});
        System.out.println(Arrays.toString(replyBytes));
        System.out.println(invoker.getProgramAttr().getName()
                + " invoked successfully");
    }
}
