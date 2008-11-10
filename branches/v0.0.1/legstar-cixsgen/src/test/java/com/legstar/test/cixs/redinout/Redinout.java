
package com.legstar.test.cixs.redinout;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.ws.RequestWrapper;
import javax.xml.ws.ResponseWrapper;


/**
 * Web service endpoint interface.
 * 
 * This class was generated by CIXS version 1.0.
 * 2007-02-06T14:30:54.562+01:00
 */

@WebService(name = "redinoutPort",
            targetNamespace = "http://cixs.test.legstar.com/redinout")
public interface Redinout {


    /**
     * Service operation redinout.
     * 
     * @param request a JAXB object mapping the request
     * @param hostHeader a JAXB object mapping header parameters
     * @return a JAXB object mapping the reply
     * @throws RedinoutFault if method fails
     */
    @WebMethod
    @WebResult(name = "Response",
               targetNamespace = "http://cixs.test.legstar.com/redinout")
    @RequestWrapper(localName = "RedinoutRequest",
               targetNamespace = "http://cixs.test.legstar.com/redinout",
               className = "com.legstar.test.cixs.redinout.RedinoutRequest")
    @ResponseWrapper(localName = "RedinoutResponse",
               targetNamespace = "http://cixs.test.legstar.com/redinout",
               className = "com.legstar.test.cixs.redinout.RedinoutResponse")
    com.legstar.test.coxb.redinout.
    DfhcommareaType redinout(
        @WebParam(name = "Request",
               targetNamespace = "http://cixs.test.legstar.com/redinout")
        com.legstar.test.coxb.redinout.
        DfhcommareaType request,
        @WebParam(name = "HostHeader", header = true, partName = "HostHeader",
                targetNamespace = "http://cixs.test.legstar.com/redinout")
        RedinoutHostHeader hostHeader)
        throws RedinoutFault;

}
