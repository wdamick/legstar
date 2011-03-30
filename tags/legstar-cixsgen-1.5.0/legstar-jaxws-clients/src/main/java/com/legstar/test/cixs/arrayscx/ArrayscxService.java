
package com.legstar.test.cixs.arrayscx;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.logging.Logger;
import javax.xml.namespace.QName;
import javax.xml.ws.Service;
import javax.xml.ws.WebEndpoint;
import javax.xml.ws.WebServiceClient;
import javax.xml.ws.WebServiceFeature;


/**
 * This class was generated by the JAX-WS RI.
 * JAX-WS RI 2.1.3-b02-
 * Generated source version: 2.1
 * 
 */
@WebServiceClient(name = "arrayscxService", targetNamespace = "http://cixs.test.legstar.com/arrayscx", wsdlLocation = "http://localhost:8080/cixs-arrayscx/arrayscx?wsdl")
public class ArrayscxService
    extends Service
{

    private final static URL ARRAYSCXSERVICE_WSDL_LOCATION;
    private final static Logger logger = Logger.getLogger(com.legstar.test.cixs.arrayscx.ArrayscxService.class.getName());

    static {
        URL url = null;
        try {
            URL baseUrl;
            baseUrl = com.legstar.test.cixs.arrayscx.ArrayscxService.class.getResource(".");
            url = new URL(baseUrl, "http://localhost:8080/cixs-arrayscx/arrayscx?wsdl");
        } catch (MalformedURLException e) {
            logger.warning("Failed to create URL for the wsdl Location: 'http://localhost:8080/cixs-arrayscx/arrayscx?wsdl', retrying as a local file");
            logger.warning(e.getMessage());
        }
        ARRAYSCXSERVICE_WSDL_LOCATION = url;
    }

    public ArrayscxService(URL wsdlLocation, QName serviceName) {
        super(wsdlLocation, serviceName);
    }

    public ArrayscxService() {
        super(ARRAYSCXSERVICE_WSDL_LOCATION, new QName("http://cixs.test.legstar.com/arrayscx", "arrayscxService"));
    }

    /**
     * 
     * @return
     *     returns ArrayscxPort
     */
    @WebEndpoint(name = "arrayscxPort")
    public ArrayscxPort getArrayscxPort() {
        return super.getPort(new QName("http://cixs.test.legstar.com/arrayscx", "arrayscxPort"), ArrayscxPort.class);
    }

    /**
     * 
     * @param features
     *     A list of {@link javax.xml.ws.WebServiceFeature} to configure on the proxy.  Supported features not in the <code>features</code> parameter will have their default values.
     * @return
     *     returns ArrayscxPort
     */
    @WebEndpoint(name = "arrayscxPort")
    public ArrayscxPort getArrayscxPort(WebServiceFeature... features) {
        return super.getPort(new QName("http://cixs.test.legstar.com/arrayscx", "arrayscxPort"), ArrayscxPort.class, features);
    }

}
