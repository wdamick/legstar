package com.legstar.test.cixs.redinout;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.ws.RequestWrapper;
import javax.xml.ws.ResponseWrapper;

import com.legstar.test.coxb.redinout.Dfhcommarea;
/**
 * LegStar/Jaxws Component interface.
 * Each method maps to a CICS program. 
 * 
 * This class was generated by LegStar Mainframe Web Service adapter generator.
 */
@WebService(name = "redinoutPort",
            targetNamespace = "http://cixs.test.legstar.com/redinout")
public interface Redinout {
  
    /**
     * LegStar operation redinout.
     * 
     * @param request a JAXB object mapping the request
     * @param hostHeader an object mapping header parameters
     * @return a JAXB object mapping the reply
     * @throws RedinoutFault if method fails
     */
    @WebMethod(operationName = "redinout", action = "urn:redinout")
    @WebResult(name = "Dfhcommarea",
        targetNamespace = "http://legstar.com/test/coxb/redinout")
    @RequestWrapper(localName = "RedinoutRequest",
        targetNamespace = "http://cixs.test.legstar.com/redinout",
        className = "com.legstar.test.cixs.redinout.RedinoutRequest")
    @ResponseWrapper(localName = "RedinoutResponse",
        targetNamespace = "http://cixs.test.legstar.com/redinout",
        className = "com.legstar.test.cixs.redinout.RedinoutResponse")
    Dfhcommarea redinout(
        @WebParam(name = "Dfhcommarea",
               targetNamespace = "http://legstar.com/test/coxb/redinout")
            Dfhcommarea request,
        @WebParam(name = "RedinoutHostHeader", header = true, partName = "hostHeader",
                targetNamespace = "http://cixs.test.legstar.com/redinout")
            RedinoutHostHeader hostHeader)
        throws RedinoutFault;
        
}