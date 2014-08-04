package com.legstar.test.cixs.arrayscx;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.ws.RequestWrapper;
import javax.xml.ws.ResponseWrapper;

import com.legstar.test.coxb.arrayscx.Dfhcommarea;
/**
 * LegStar/Jaxws Component interface.
 * Each method maps to a CICS program. 
 * 
 * This class was generated by LegStar Mainframe Web Service adapter generator.
 */
@WebService(name = "arrayscxPort",
            targetNamespace = "http://cixs.test.legstar.com/arrayscx")
public interface Arrayscx {
  
    /**
     * LegStar operation arrayscx.
     * 
     * @param request a JAXB object mapping the request
     * @param hostHeader an object mapping header parameters
     * @return a JAXB object mapping the reply
     * @throws ArrayscxFault if method fails
     */
    @WebMethod(operationName = "arrayscx", action = "urn:arrayscx")
    @WebResult(name = "Dfhcommarea",
        targetNamespace = "http://legstar.com/test/coxb/arrayscx")
    @RequestWrapper(localName = "ArrayscxRequest",
        targetNamespace = "http://cixs.test.legstar.com/arrayscx",
        className = "com.legstar.test.cixs.arrayscx.ArrayscxRequest")
    @ResponseWrapper(localName = "ArrayscxResponse",
        targetNamespace = "http://cixs.test.legstar.com/arrayscx",
        className = "com.legstar.test.cixs.arrayscx.ArrayscxResponse")
    Dfhcommarea arrayscx(
        @WebParam(name = "Dfhcommarea",
               targetNamespace = "http://legstar.com/test/coxb/arrayscx")
            Dfhcommarea request,
        @WebParam(name = "ArrayscxHostHeader", header = true, partName = "hostHeader",
                targetNamespace = "http://cixs.test.legstar.com/arrayscx")
            ArrayscxHostHeader hostHeader)
        throws ArrayscxFault;
        
}
