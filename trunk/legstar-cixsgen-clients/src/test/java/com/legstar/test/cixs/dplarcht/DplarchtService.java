
package com.legstar.test.cixs.dplarcht;

import java.net.MalformedURLException;
import java.net.URL;
import javax.xml.namespace.QName;
import javax.xml.ws.Service;
import javax.xml.ws.WebEndpoint;
import javax.xml.ws.WebServiceClient;
import javax.xml.ws.WebServiceFeature;


/**
 * This class was generated by the JAXWS SI.
 * JAX-WS RI 2.1-01/26/2007 12:09 AM(kohsuke)-RC2
 * Generated source version: 2.1
 * 
 */
@WebServiceClient(name = "dplarchtService", targetNamespace = "http://cixs.test.legstar.com/dplarcht", wsdlLocation = "http://localhost:8080/cixs-dplarcht/dplarcht?wsdl")
public class DplarchtService
    extends Service
{

    private final static URL DPLARCHTSERVICE_WSDL_LOCATION;

    static {
        URL url = null;
        try {
            url = new URL("http://localhost:8080/cixs-dplarcht/dplarcht?wsdl");
        } catch (MalformedURLException e) {
            e.printStackTrace();
        }
        DPLARCHTSERVICE_WSDL_LOCATION = url;
    }

    public DplarchtService(URL wsdlLocation, QName serviceName) {
        super(wsdlLocation, serviceName);
    }

    public DplarchtService() {
        super(DPLARCHTSERVICE_WSDL_LOCATION, new QName("http://cixs.test.legstar.com/dplarcht", "dplarchtService"));
    }

    /**
     * 
     * @return
     *     returns DplarchtPort
     */
    @WebEndpoint(name = "DplarchtImplPort")
    public DplarchtPort getDplarchtImplPort() {
        return (DplarchtPort)super.getPort(new QName("http://cixs.test.legstar.com/dplarcht", "DplarchtImplPort"), DplarchtPort.class);
    }

    /**
     * 
     * @param features
     *     A list of {@link javax.xml.ws.WebServiceFeature} to configure on the proxy.  Supported features not in the <code>features</code> parameter will have their default values.
     * @return
     *     returns DplarchtPort
     */
    @WebEndpoint(name = "DplarchtImplPort")
    public DplarchtPort getDplarchtImplPort(WebServiceFeature... features) {
        return (DplarchtPort)super.getPort(new QName("http://cixs.test.legstar.com/dplarcht", "DplarchtImplPort"), DplarchtPort.class, features);
    }

}
