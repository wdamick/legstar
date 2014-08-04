
package com.legstar.test.cixs.lsfileal;

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
@WebServiceClient(name = "lsfilealService", targetNamespace = "http://cixs.test.legstar.com/lsfileal", wsdlLocation = "http://localhost:8080/cixs-lsfileal/lsfileal?wsdl")
public class LsfilealService
    extends Service
{

    private final static URL LSFILEALSERVICE_WSDL_LOCATION;
    private final static Logger logger = Logger.getLogger(com.legstar.test.cixs.lsfileal.LsfilealService.class.getName());

    static {
        URL url = null;
        try {
            URL baseUrl;
            baseUrl = com.legstar.test.cixs.lsfileal.LsfilealService.class.getResource(".");
            url = new URL(baseUrl, "http://localhost:8080/cixs-lsfileal/lsfileal?wsdl");
        } catch (MalformedURLException e) {
            logger.warning("Failed to create URL for the wsdl Location: 'http://localhost:8080/cixs-lsfileal/lsfileal?wsdl', retrying as a local file");
            logger.warning(e.getMessage());
        }
        LSFILEALSERVICE_WSDL_LOCATION = url;
    }

    public LsfilealService(URL wsdlLocation, QName serviceName) {
        super(wsdlLocation, serviceName);
    }

    public LsfilealService() {
        super(LSFILEALSERVICE_WSDL_LOCATION, new QName("http://cixs.test.legstar.com/lsfileal", "lsfilealService"));
    }

    /**
     * 
     * @return
     *     returns LsfilealPort
     */
    @WebEndpoint(name = "lsfilealPort")
    public LsfilealPort getLsfilealPort() {
        return super.getPort(new QName("http://cixs.test.legstar.com/lsfileal", "lsfilealPort"), LsfilealPort.class);
    }

    /**
     * 
     * @param features
     *     A list of {@link javax.xml.ws.WebServiceFeature} to configure on the proxy.  Supported features not in the <code>features</code> parameter will have their default values.
     * @return
     *     returns LsfilealPort
     */
    @WebEndpoint(name = "lsfilealPort")
    public LsfilealPort getLsfilealPort(WebServiceFeature... features) {
        return super.getPort(new QName("http://cixs.test.legstar.com/lsfileal", "lsfilealPort"), LsfilealPort.class, features);
    }

}
