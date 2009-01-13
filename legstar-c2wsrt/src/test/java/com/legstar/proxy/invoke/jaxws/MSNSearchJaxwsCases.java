package com.legstar.proxy.invoke.jaxws;

import java.util.HashMap;
import java.util.Map;

import com.legstar.coxb.transform.HostTransformException;
import com.legstar.proxy.invoke.DirectOperationProxy;
import com.legstar.proxy.invoke.IProxyInvoker;
import com.legstar.proxy.invoke.ReflectOperationProxy;
import com.legstar.test.coxb.MSNSearchCases;
import com.legstar.test.coxb.MSNSearch.SearchResponse;
import com.legstar.test.coxb.MSNSearch.bind.SearchResponseHostToJavaTransformer;

import junit.framework.TestCase;

/**
 * Centralize data for Cultureinfo test case.
 *
 */
public class MSNSearchJaxwsCases extends TestCase {


    /**
     * @return a valid proxy configuration
     */
    private static Map < String, String > getConfig() {
        Map < String, String > config = new HashMap < String, String >();

        config.put(IProxyInvoker.PROXY_INVOKER_CLASS_NAME_PROPERTY,
                "com.legstar.proxy.invoke.jaxws.WebServiceInvoker");

        config.put(WebServiceInvoker.WSDL_URL_PROPERTY,
                "http://soap.search.msn.com/webservices.asmx?wsdl");
        config.put(WebServiceInvoker.WSDL_TARGET_NAMESPACE_PROPERTY,
                "http://schemas.microsoft.com/MSNSearch/2005/09/fex");
        config.put(WebServiceInvoker.WSDL_SERVICE_NAME_PROPERTY,
                "MSNSearchService");
        config.put(WebServiceInvoker.WSDL_PORT_NAME_PROPERTY,
                "MSNSearchPort");
        return config;
    }

    /**
     * @return a complete configuration that result in direct proxying.
     */
    public static Map < String, String > getDirectConfig() {
        Map < String, String > config = getReflectConfig();
        config.put(DirectOperationProxy.REQUEST_TRANSFORMERS_CLASS_NAME_PROPERTY,
        "com.legstar.test.coxb.MSNSearch.bind.SearchTransformers");
        config.put(DirectOperationProxy.RESPONSE_TRANSFORMERS_CLASS_NAME_PROPERTY,
        "com.legstar.test.coxb.MSNSearch.bind.SearchResponseTransformers");
        return config;
    }

    /**
     * @return a complete configuration that result in reflection proxying.
     */
    public static Map < String, String > getReflectConfig() {
        Map < String, String > config = getConfig();
        config.put(ReflectOperationProxy.REQUEST_JAXB_TYPE_PROPERTY,
        "Search");
        config.put(ReflectOperationProxy.REQUEST_JAXB_PACKAGE_NAME_PROPERTY,
        "com.legstar.test.coxb.MSNSearch");
        config.put(ReflectOperationProxy.RESPONSE_JAXB_TYPE_PROPERTY,
        "SearchResponse");
        config.put(ReflectOperationProxy.RESPONSE_JAXB_PACKAGE_NAME_PROPERTY,
        "com.legstar.test.coxb.MSNSearch");
        return config;
    }
    /**
     * Check the response.
     * @param replyBytes the host bytes returned
     */
    public static void checkHostBytesResponse(final byte[] replyBytes) {
        try {
            SearchResponseHostToJavaTransformer transformer = new SearchResponseHostToJavaTransformer();
            SearchResponse searchResponse = transformer.transform(replyBytes);
            MSNSearchCases.checkJavaObjectResponse(searchResponse);
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }

}
