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
package com.legstar.test.cixs;

import com.legstar.test.cixs.varar021.Varar021Fault;
import com.legstar.test.cixs.varar021.Varar021HostHeader;
import com.legstar.test.cixs.varar021.Varar021Impl;
import com.legstar.test.coxb.Varar021Cases;
import com.legstar.test.coxb.varar021.IStaticData;
import com.legstar.test.coxb.varar021.LkupInfo39;
import com.legstar.test.coxb.varar021.Payload;
import com.legstar.test.coxb.varar021.SearchGrplst;

import junit.framework.TestCase;

/**
 * Test VARAR021 adapter.
 *
 */
public class Varar021Test extends TestCase {
    
    /**
     * All default value should behave like echo.
     * @throws Varar021Fault if test fails
     */
    public void testVarar021Empty() throws Varar021Fault {
        Varar021Impl port = new Varar021Impl();
        SearchGrplst request = new SearchGrplst();
        SearchGrplst reply = port.varar021(request, null);
        Varar021Cases.checkJavaObjectEmpty(reply);
    }

    /**
     * Send I-STATIC-DATA with data that generates an error reply back.
     * @throws Varar021Fault if test fails
     */
    public void testVarar021Error() throws Varar021Fault {
        Varar021Impl port = new Varar021Impl();
        SearchGrplst request = new SearchGrplst();
        Payload payload = new Payload();
        IStaticData iStaticData = new IStaticData();
        iStaticData.getLkupInfo().add(new LkupInfo39());
        iStaticData.getLkupInfo().add(new LkupInfo39());
        iStaticData.getLkupInfo().get(0).setLkupTypCd("ALPHA");
        payload.setIStaticData(iStaticData);
        request.setPayload(payload);
        SearchGrplst reply = port.varar021(request, null);
        Varar021Cases.checkJavaObjectError(reply);
    }

    /**
     * Send I-STATIC-DATA with data that generates a number of valid
     * O-DYNAMIC-DATA structures back.
     * @throws Varar021Fault if test fails
     */
    public void testVarar02110Items() throws Varar021Fault {
        Varar021Impl port = new Varar021Impl();
        SearchGrplst request = new SearchGrplst();
        Payload payload = new Payload();
        IStaticData iStaticData = new IStaticData();
        iStaticData.getLkupInfo().add(new LkupInfo39());
        iStaticData.getLkupInfo().add(new LkupInfo39());
        iStaticData.getLkupInfo().get(0).setLkupTypCd("00010");
        payload.setIStaticData(iStaticData);
        request.setPayload(payload);
        SearchGrplst reply = port.varar021(request, null);
        Varar021Cases.checkJavaObject10Items(reply);
    }

    /**
     * Try overriding the invoker configuration parameters.
     * This should pickup an alternate host endpoint which is password
     * protected. Passing the wrong credentials should result in an
     * explicit exception.
     */
    public void testVarar021OverrideParameters() {
        Varar021Impl port = new Varar021Impl();
        SearchGrplst request = new SearchGrplst();
        Varar021HostHeader hostHeader = new Varar021HostHeader();
        hostHeader.setHostEndPoint("CICSTS31");
        hostHeader.setHostUserID("WRONG");
        hostHeader.setHostPassword("WRONG");
        try {
            port.varar021(request, hostHeader);
            fail();
        } catch (Varar021Fault e) {
            assertTrue(e.getMessage().contains("Failed to invoke host program:"));
            assertTrue(e.getMessage().contains(
                    "SEE06908 The USERID is not known to the external security manager."));
            assertTrue(e.getFaultInfo().getMessage().contains(
                    "SEE06908 The USERID is not known to the external security manager."));
            assertEquals("Operation=varar021,"
                    + " Program properties={"
                    + "\"CICSProgramName\":\"VARAR021\","
                    + "\"CICSLength\":19922,"
                    + "\"CICSDataLength\":19922},"
                    + " Config file name=legstar-invoker-config.xml,"
                    + " HostInvoker=["
                    + "Address=["
                    + "hostEndPoint=CICSTS31,"
                    + "hostCharset=IBM01140,"
                    + "hostUserID=WRONG,"
                    + "hostTraceMode=true],"
                    + "HostAccessStrategy=com.legstar.host.access.DirectHostAccessStrategy,"
                    + "{\"CICSProgramName\":\"VARAR021\","
                    + "\"CICSLength\":19922,"
                    + "\"CICSDataLength\":19922}]",
                    e.getFaultInfo().getDetail());
        }
    }
 
    /**
     * Client should be able to provide its own unique requestID.
     * @throws Varar021Fault if test fails
     */
    public void testVarar021OverrideRequestID() throws Varar021Fault {
        Varar021Impl port = new Varar021Impl();
        /* First make sure we get a default request ID*/
        assertTrue(port.getRequestID(null).contains("varar021:"));
        /* Now try with a new request ID */
        Varar021HostHeader hostHeader = new Varar021HostHeader();
        hostHeader.setHostRequestID("benicio");
        assertEquals("benicio", port.getRequestID(hostHeader));
    }

}
