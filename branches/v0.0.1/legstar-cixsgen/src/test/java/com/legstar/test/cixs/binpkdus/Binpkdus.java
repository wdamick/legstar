
package com.legstar.test.cixs.binpkdus;
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
 * 2007-02-06T14:30:38.218+01:00
 */

@WebService(name = "binpkdusPort",
            targetNamespace = "http://cixs.test.legstar.com/binpkdus")
public interface Binpkdus {


    /**
     * Service operation binpkdus.
     * 
     * @param request a JAXB object mapping the request
     * @param hostHeader a JAXB object mapping header parameters
     * @return a JAXB object mapping the reply
     * @throws BinpkdusFault if method fails
     */
    @WebMethod
    @WebResult(name = "Response",
               targetNamespace = "http://cixs.test.legstar.com/binpkdus")
    @RequestWrapper(localName = "BinpkdusRequest",
               targetNamespace = "http://cixs.test.legstar.com/binpkdus",
               className = "com.legstar.test.cixs.binpkdus.BinpkdusRequest")
    @ResponseWrapper(localName = "BinpkdusResponse",
               targetNamespace = "http://cixs.test.legstar.com/binpkdus",
               className = "com.legstar.test.cixs.binpkdus.BinpkdusResponse")
    com.legstar.test.coxb.binpkdus.
    DfhcommareaType binpkdus(
        @WebParam(name = "Request",
               targetNamespace = "http://cixs.test.legstar.com/binpkdus")
        com.legstar.test.coxb.binpkdus.
        DfhcommareaType request,
        @WebParam(name = "HostHeader", header = true, partName = "HostHeader",
                targetNamespace = "http://cixs.test.legstar.com/binpkdus")
        BinpkdusHostHeader hostHeader)
        throws BinpkdusFault;

}
