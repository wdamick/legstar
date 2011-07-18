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
package com.legstar.proxy.invoke.jaxws;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

import com.legstar.proxy.invoke.AbstractWebServiceTest;
import com.legstar.test.coxb.cultureinfo.CultureInfoParameters;
import com.legstar.test.coxb.cultureinfo.GetInfo;
import com.legstar.test.coxb.cultureinfo.GetInfoResponse;

/**
 * Test WebServiceInvoker.
 * 
 */
public class WebServiceInvokerTest extends AbstractWebServiceTest {

    /**
     * Test configuration parameters.
     */
    public void testConfig() {
        Map < String, String > config = new HashMap < String, String >();
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals(
                    "You must specify a wsdl URL using the wsdlUrl attribute",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.WSDL_URL_PROPERTY, "http://org.url");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals(
                    "You must specify a wsdl target namespace using the wsdlTargetNamespace attribute",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.WSDL_TARGET_NAMESPACE_PROPERTY,
                "http://org.url");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals(
                    "You must specify a service name using the wsdlServiceName attribute",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.WSDL_SERVICE_NAME_PROPERTY, "serviceName");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals(
                    "You must specify a port name using the wsdlPortName attribute",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.WSDL_PORT_NAME_PROPERTY, "portName");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals(
                    "You must specify a jaxb request type using the requestJaxbType attribute",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.REQUEST_JAXB_TYPE_PROPERTY, "jaxbType");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals(
                    "You must specify a jaxb request package name using the requestJaxbPackageName attribute",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.REQUEST_JAXB_PACKAGE_NAME_PROPERTY,
                "jaxbPackage");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals(
                    "You must specify a jaxb response type using the responseJaxbType attribute",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.RESPONSE_JAXB_TYPE_PROPERTY, "jaxbType");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals(
                    "You must specify a jaxb response package name using the responseJaxbPackageName attribute",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.RESPONSE_JAXB_PACKAGE_NAME_PROPERTY,
                "jaxbPackage");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals(
                    "com.legstar.coxb.util.JAXBAnnotationException: java.lang.ClassNotFoundException:"
                            + " jaxbPackage.ObjectFactory", e.getMessage());
        }
        config.put(WebServiceInvoker.REQUEST_JAXB_TYPE_PROPERTY,
                "CultureInfoParameters");
        config.put(WebServiceInvoker.REQUEST_JAXB_PACKAGE_NAME_PROPERTY,
                "com.legstar.test.coxb.cultureinfo");
        try {
            new WebServiceInvoker(config);
            fail();
        } catch (WebServiceInvokerException e) {
            assertEquals(
                    "com.legstar.coxb.util.JAXBAnnotationException: java.lang.ClassNotFoundException:"
                            + " jaxbPackage.ObjectFactory", e.getMessage());
        }
        config.put(WebServiceInvoker.RESPONSE_JAXB_TYPE_PROPERTY,
                "CultureInfoReply");
        config.put(WebServiceInvoker.RESPONSE_JAXB_PACKAGE_NAME_PROPERTY,
                "com.legstar.test.coxb.cultureinfo");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            fail(e.getMessage());
        }

    }

    /**
     * Test the dispatch method.
     * 
     * @throws Exception if test fails
     */
    public void testInvokeCultureinfo() throws Exception {

        /* Setup a configuration and instantiate action. */
        WebServiceInvoker invoker = new WebServiceInvoker(
                CultureinfoJaxwsCases.getReflectConfig());

        for (int i = 0; i < 5; i++) {
            GetInfoResponse reply = invoker.invoke(getName(),
                    getCultureInfoRequest());
            assertEquals("fr-FR", reply.getReturn().getServerCultureInfo()
                    .getCultureCode());
        }
    }

    /**
     * @return a sample Cultureinfo request
     */
    public static GetInfo getCultureInfoRequest() {
        GetInfo wrapper = new GetInfo();
        CultureInfoParameters request = new CultureInfoParameters();
        request.setCultureCode("fr-FR");
        request.setDecimalNumber(BigDecimal.valueOf(275.36));
        wrapper.setArg0(request);
        return wrapper;
    }

    /**
     * Check the dispatcher caching mechanism.
     * 
     * @FIXME Does not work with Cargo (Not related with Dispatcher reuse it
     *        seems?)
     * 
     * @throws Exception when test fails
     */
    public void testWebServiceInvokerReuse() throws Exception {
        /* Setup a configuration and instantiate action. */
        WebServiceInvoker invoker = new WebServiceInvoker(
                CultureinfoJaxwsCases.getReflectConfig());

        int threadsSize = 5;
        Thread[] threads = new Thread[threadsSize];
        WebServiceInvokerRunnable[] runnables = new WebServiceInvokerRunnable[threadsSize];
        for (int i = 0; i < threadsSize; i++) {
            runnables[i] = new WebServiceInvokerRunnable(invoker);
            threads[i] = new Thread(runnables[i]);
        }
        for (int i = 0; i < threadsSize; i++) {
            threads[i].start();
        }
        for (int i = 0; i < threadsSize; i++) {
            threads[i].join();
        }
        for (int i = 0; i < threadsSize; i++) {
            assertTrue(runnables[i].getException() == null);
        }

    }

    /**
     * Class used for testing sharing invokers between threads.
     * 
     */
    public class WebServiceInvokerRunnable implements Runnable {

        /** A shared invoker. */
        private WebServiceInvoker _invoker;

        /** Any exception caught by this thread. */
        private Throwable _exception;

        /**
         * @param invoker the shared invoker
         */
        public WebServiceInvokerRunnable(final WebServiceInvoker invoker) {
            _invoker = invoker;
        }

        /** {@inheritDoc} */
        public void run() {
            try {
                String id = Thread.currentThread().getName();
                for (int i = 0; i < 5; i++) {

                    GetInfoResponse reply = _invoker.invoke(id,
                            getCultureInfoRequest());
                    assertEquals("fr-FR", reply.getReturn()
                            .getServerCultureInfo().getCultureCode());
                }
            } catch (Exception e) {
                e.printStackTrace();
                _exception = e;
            }
        }

        /**
         * @return any exception caught by this thread
         */
        public Throwable getException() {
            return _exception;
        }

    }

}
