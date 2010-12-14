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

import java.util.HashMap;
import java.util.Map;

import com.legstar.test.coxb.MSNSearch.ArrayOfSourceRequestRequestsType;
import com.legstar.test.coxb.MSNSearch.Search;
import com.legstar.test.coxb.MSNSearch.SearchRequestType;
import com.legstar.test.coxb.MSNSearch.SearchResponse;
import com.legstar.test.coxb.MSNSearch.SourceRequestType;
import com.legstar.test.coxb.MSNSearch.SourceTypeType;

import junit.framework.TestCase;

/**
 * Test WebServiceInvoker.
 * 
 */
public class WebServiceInvokerTest extends TestCase {

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
                            + " jaxbPackage.ObjectFactory",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.REQUEST_JAXB_TYPE_PROPERTY, "Search");
        config.put(WebServiceInvoker.REQUEST_JAXB_PACKAGE_NAME_PROPERTY,
                "com.legstar.test.coxb.MSNSearch");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals(
                    "com.legstar.coxb.util.JAXBAnnotationException: java.lang.ClassNotFoundException:"
                            + " jaxbPackage.ObjectFactory",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.RESPONSE_JAXB_TYPE_PROPERTY,
                "SearchResponse");
        config.put(WebServiceInvoker.RESPONSE_JAXB_PACKAGE_NAME_PROPERTY,
                "com.legstar.test.coxb.MSNSearch");
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
    public void testInvokeMSNSearch() throws Exception {

        /* Setup a configuration and instantiate action. */
        WebServiceInvoker invoker = new WebServiceInvoker(MSNSearchJaxwsCases
                .getReflectConfig());

        /* Invoke the web service several times. */
        try {
            for (int i = 0; i < 5; i++) {
                SearchResponse searchResponse = invoker.invoke(getName(),
                        getSearchRequest());
                assertTrue(searchResponse.getResponse().getResponses()
                        .getSourceResponse().size() > 0);
            }

        } catch (Exception e) {
            fail(e.getMessage());
        }

    }

    /**
     * @return a sample MSNSearch request
     */
    public static Search getSearchRequest() {
        /* Create a JAXB request object. */
        com.legstar.test.coxb.MSNSearch.ObjectFactory jaxbFactory =
                new com.legstar.test.coxb.MSNSearch.ObjectFactory();

        SourceRequestType sourceRequestType = new SourceRequestType();
        sourceRequestType.setSource(SourceTypeType.WEB);
        sourceRequestType.setCount(10);

        ArrayOfSourceRequestRequestsType sr = jaxbFactory
                .createArrayOfSourceRequestRequestsType();
        sr.getSourceRequest().add(sourceRequestType);

        SearchRequestType searchRequestType = jaxbFactory
                .createSearchRequestType();
        searchRequestType.setAppID("5588C3ACE949315B3ECAADDA908611BDF5D8D5AA");
        searchRequestType.setRequests(sr);
        searchRequestType.setQuery("LegStar");
        searchRequestType.setCultureInfo("en-US");

        Search search = jaxbFactory.createSearch();
        search.setRequest(searchRequestType);
        return search;
    }

    /**
     * Check the dispatcher caching mechanism.
     * Have to disable this one. MSN does not like it anymore.
     * 
     * @throws Exception when test fails
     */
    public void notestWebServiceInvokerReuse() throws Exception {
        /* Setup a configuration and instantiate action. */
        WebServiceInvoker invoker = new WebServiceInvoker(MSNSearchJaxwsCases
                .getReflectConfig());

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
                SearchResponse searchResponse = _invoker.invoke(id,
                        getSearchRequest());
                assertTrue(searchResponse.getResponse().getResponses()
                        .getSourceResponse().size() > 0);
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
