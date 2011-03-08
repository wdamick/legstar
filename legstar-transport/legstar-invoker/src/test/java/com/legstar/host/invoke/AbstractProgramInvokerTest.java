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
package com.legstar.host.invoke;

import com.legstar.messaging.LegStarAddress;

import junit.framework.TestCase;

/**
 * Test AbstractProgramInvoker.
 *
 */
public class AbstractProgramInvokerTest extends TestCase {
    
    /**
     * Test how host invokers are reused.
     */
    public void testGetHostInvoker() {
        try {
            AbstractProgramInvoker programInvoker = new AbstractProgramInvokerImpl();
            assertEquals("Operation=lsfileae,"
                    + " Program properties={"
                    + "\"CICSProgramName\":\"LSFILEAE\","
                    + "\"CICSLength\":79,"
                    + "\"CICSDataLength\":6},"
                    + " Config file name=config0.xml",
                    programInvoker.toString());
            LegStarAddress address = new LegStarAddress("TheMainframe");
            HostInvoker hostInvoker1 = programInvoker.getHostInvoker(address);
            assertEquals("Operation=lsfileae,"
                    + " Program properties={"
                    + "\"CICSProgramName\":\"LSFILEAE\","
                    + "\"CICSLength\":79,"
                    + "\"CICSDataLength\":6},"
                    + " Config file name=config0.xml,"
                    + " HostInvoker=["
                    + "Address=["
                    + "hostEndpoint=TheMainframe,"
                    + "hostCharset=IBM01140,"
                    + "hostUserID=P390,"
                    + "hostTraceMode=true],"
                    + "HostAccessStrategy=com.legstar.host.access.DirectHostAccessStrategy,"
                    + "{\"CICSProgramName\":\"LSFILEAE\","
                    + "\"CICSLength\":79,"
                    + "\"CICSDataLength\":6}]",
                    programInvoker.toString());
            HostInvoker hostInvoker2 = programInvoker.getHostInvoker(address);
            assertEquals(hostInvoker1, hostInvoker2);
            HostInvoker hostInvoker3 = programInvoker.getHostInvoker(
                    new LegStarAddress("CICSTS31"));
            assertNotSame(hostInvoker1, hostInvoker3);
            assertEquals("Operation=lsfileae,"
                    + " Program properties={"
                    + "\"CICSProgramName\":\"LSFILEAE\","
                    + "\"CICSLength\":79,"
                    + "\"CICSDataLength\":6},"
                    + " Config file name=config0.xml,"
                    + " HostInvoker=["
                    + "Address=["
                    + "hostEndpoint=CICSTS31,"
                    + "hostCharset=IBM01140,"
                    + "hostUserID=P390,"
                    + "hostTraceMode=true],"
                    + "HostAccessStrategy=com.legstar.host.access.DirectHostAccessStrategy,"
                    + "{\"CICSProgramName\":\"LSFILEAE\","
                    + "\"CICSLength\":79,"
                    + "\"CICSDataLength\":6}],"
                    + " HostInvoker=["
                    + "Address=["
                    + "hostEndpoint=TheMainframe,"
                    + "hostCharset=IBM01140,"
                    + "hostUserID=P390,"
                    + "hostTraceMode=true],"
                    + "HostAccessStrategy=com.legstar.host.access.DirectHostAccessStrategy,"
                    + "{\"CICSProgramName\":\"LSFILEAE\","
                    + "\"CICSLength\":79,"
                    + "\"CICSDataLength\":6}]",
                    programInvoker.toString());
        } catch (HostInvokerException e) {
            fail(e.toString());
        }
        
    }
    
    /**
     * A test implementation of a program invoker.
     *
     */
    public class AbstractProgramInvokerImpl extends AbstractProgramInvoker {
        

        /**
         * An implementation of a program invoker.
         */
        public AbstractProgramInvokerImpl() {
            super("config0.xml", "lsfileae", "lsfileae.properties");
        }
        
    }

}
