package com.legstar.test.cixs;

import com.legstar.coxb.transform.IHostToJavaTransformer;
import com.legstar.coxb.transform.IJavaToHostTransformer;
import com.legstar.host.invoke.HostInvoker;
import com.legstar.test.cixs.varar021.Varar021Fault;
import com.legstar.test.cixs.varar021.Varar021HostHeader;
import com.legstar.test.cixs.varar021.Varar021Impl;
import com.legstar.test.coxb.Varar021Cases;
import com.legstar.test.coxb.varar021.IStaticData;
import com.legstar.test.coxb.varar021.LkupInfo36;
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
        iStaticData.getLkupInfo().add(new LkupInfo36());
        iStaticData.getLkupInfo().add(new LkupInfo36());
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
        iStaticData.getLkupInfo().add(new LkupInfo36());
        iStaticData.getLkupInfo().add(new LkupInfo36());
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
                    + "HostInvoker=[Address=[hostEndPoint=CICSTS31,"
                    + "hostCharset=IBM01140,"
                    + "hostUserID=WRONG,"
                    + "hostTraceMode=true],"
                    + "HostAccessStrategy=com.legstar.host.access.DirectHostAccessStrategy,"
                    + "CicsProgram=[CICSProgramName=VARAR021,"
                    + "CICSLength=19922,"
                    + "CICSDataLength=19922,"
                    + "CICSSysID=null,"
                    + "CICSSyncOnReturn=false,"
                    + "CICSTransID=null,"
                    + "CICSChannel=null]]",
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

    /**
     * Client should be able to provide its own host character set.
     * @throws Varar021Fault if test fails
     */
    public void testVarar021OverrideHostCharset() throws Varar021Fault {
        Varar021Impl port = new Varar021Impl();
        SearchGrplst request = new SearchGrplst();
        /* First make sure we get a default host character set*/
        port.varar021(request, null);
        assertEquals("IBM01140", port.getVarar021OperationInvoker().getHostInvoker().getAddress().getHostCharset());
        HostInvoker invokerInstance = port.getVarar021OperationInvoker().getHostInvoker();
        IJavaToHostTransformer javaToHostInstance =
            port.getVarar021OperationInvoker().getSearchGrplstTransformers().getJavaToHost();
        IHostToJavaTransformer hostToJavaInstance =
            port.getVarar021OperationInvoker().getSearchGrplstTransformers().getHostToJava();

        /* If we call the operation a second time without changing the character set,
         * we should reuse the same instance of invoker and transformers. */
        port.varar021(request, null);
        assertEquals("IBM01140", port.getVarar021OperationInvoker().getHostInvoker().getAddress().getHostCharset());
        assertEquals(invokerInstance, port.getVarar021OperationInvoker().getHostInvoker());
        assertEquals(javaToHostInstance,
                port.getVarar021OperationInvoker().getSearchGrplstTransformers().getJavaToHost());
        assertEquals(hostToJavaInstance,
                port.getVarar021OperationInvoker().getSearchGrplstTransformers().getHostToJava());
        
        /* Now try with a new host character set. The host invoker should be different since the address
         * has changed (the host character set is part of the address). The transformers should be
         * the same as they can adapt to a new host character set. */
        Varar021HostHeader hostHeader = new Varar021HostHeader();
        hostHeader.setHostCharset("IBM01147");
        port.varar021(request, hostHeader);
        assertEquals("IBM01147", port.getVarar021OperationInvoker().getHostInvoker().getAddress().getHostCharset());
        assertNotSame(invokerInstance, port.getVarar021OperationInvoker().getHostInvoker());
        assertEquals(javaToHostInstance,
                port.getVarar021OperationInvoker().getSearchGrplstTransformers().getJavaToHost());
        assertEquals(hostToJavaInstance,
                port.getVarar021OperationInvoker().getSearchGrplstTransformers().getHostToJava());

    }
}
