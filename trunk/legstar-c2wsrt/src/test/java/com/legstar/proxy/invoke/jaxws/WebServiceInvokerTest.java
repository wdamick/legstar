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
            assertEquals("You must specify a wsdl URL using the wsdlUrl attribute", e.getMessage());
        }
        config.put(WebServiceInvoker.WSDL_URL_PROPERTY, "http://org.url");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals("You must specify a wsdl target namespace using the wsdlTargetNamespace attribute",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.WSDL_TARGET_NAMESPACE_PROPERTY, "http://org.url");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals("You must specify a service name using the wsdlServiceName attribute",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.WSDL_SERVICE_NAME_PROPERTY, "serviceName");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals("You must specify a port name using the wsdlPortName attribute",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.WSDL_PORT_NAME_PROPERTY, "portName");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals("You must specify a jaxb request type using the requestJaxbType attribute",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.REQUEST_JAXB_TYPE_PROPERTY, "jaxbType");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals("You must specify a jaxb request package name using the requestJaxbPackageName attribute",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.REQUEST_JAXB_PACKAGE_NAME_PROPERTY, "jaxbPackage");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals("com.legstar.util.JAXBAnnotationException: java.lang.ClassNotFoundException:"
                    + " jaxbPackage.jaxbType",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.REQUEST_JAXB_TYPE_PROPERTY, "Search");
        config.put(WebServiceInvoker.REQUEST_JAXB_PACKAGE_NAME_PROPERTY,
                "com.legstar.test.coxb.MSNSearch");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals("You must specify a jaxb response type using the responseJaxbType attribute",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.RESPONSE_JAXB_TYPE_PROPERTY, "jaxbType");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals("You must specify a jaxb response package name using the responseJaxbPackageName attribute",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.RESPONSE_JAXB_PACKAGE_NAME_PROPERTY, "jaxbPackage");
        try {
            new WebServiceInvoker(config);
        } catch (WebServiceInvokerException e) {
            assertEquals("com.legstar.util.JAXBAnnotationException: java.lang.ClassNotFoundException:"
                    + " jaxbPackage.jaxbType",
                    e.getMessage());
        }
        config.put(WebServiceInvoker.RESPONSE_JAXB_TYPE_PROPERTY, "SearchResponse");
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
     * @throws Exception if test fails
     */
    public void testInvokeMSNSearch() throws Exception {
        
        /* Setup a configuration and instantiate action. */
        WebServiceInvoker invoker = new WebServiceInvoker(MSNSearchJaxwsCases.getReflectConfig());
        
        /* Create a JAXB request object. */
        com.legstar.test.coxb.MSNSearch.ObjectFactory jaxbFactory =
            new com.legstar.test.coxb.MSNSearch.ObjectFactory();

        SourceRequestType sourceRequestType = new SourceRequestType();
        sourceRequestType.setSource(SourceTypeType.WEB);
        sourceRequestType.setCount(10);
        
        ArrayOfSourceRequestRequestsType sr = jaxbFactory.createArrayOfSourceRequestRequestsType();
        sr.getSourceRequest().add(sourceRequestType);

        SearchRequestType searchRequestType = jaxbFactory.createSearchRequestType();
        searchRequestType.setAppID("5588C3ACE949315B3ECAADDA908611BDF5D8D5AA");
        searchRequestType.setRequests(sr);
        searchRequestType.setQuery("LegStar");
        searchRequestType.setCultureInfo("en-US");

        Search search = jaxbFactory.createSearch();
        search.setRequest(searchRequestType);

        /* Invoke the web service. */
        try {
            SearchResponse searchResponse = invoker.invoke(getName(), search);
            assertTrue(searchResponse.getResponse().getResponses().getSourceResponse().size() > 0);
            
        } catch (Exception e) {
            fail(e.getMessage());
        }
       
    }
    

}
