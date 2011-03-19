package com.legstar.test.cixs.lsfileae;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.ws.RequestWrapper;
import javax.xml.ws.ResponseWrapper;

import com.legstar.test.coxb.lsfileae.Dfhcommarea;
/**
 * LegStar/Jaxws Component interface.
 * Each method maps to a CICS program. 
 * 
 * This class was generated by LegStar Mainframe Web Service adapter generator.
 */
@WebService(name = "alltypesPort",
            targetNamespace = "http://cixs.test.legstar.com/alltypes")
public interface Lsfileae {
  
    /**
     * LegStar operation lsfileae.
     * 
     * @param request a JAXB object mapping the request
     * @param hostHeader an object mapping header parameters
     * @return a JAXB object mapping the reply
     * @throws LsfileaeFault if method fails
     */
    @WebMethod(operationName = "lsfileae", action = "urn:lsfileae")
    @WebResult(name = "Dfhcommarea",
        targetNamespace = "http://legstar.com/test/coxb/lsfileae")
    @RequestWrapper(localName = "LsfileaeRequest",
        targetNamespace = "http://cixs.test.legstar.com/alltypes",
        className = "com.legstar.test.cixs.lsfileae.LsfileaeRequest")
    @ResponseWrapper(localName = "LsfileaeResponse",
        targetNamespace = "http://cixs.test.legstar.com/alltypes",
        className = "com.legstar.test.cixs.lsfileae.LsfileaeResponse")
    Dfhcommarea lsfileae(
        @WebParam(name = "Dfhcommarea",
               targetNamespace = "http://legstar.com/test/coxb/lsfileae")
            Dfhcommarea request,
        @WebParam(name = "LsfileaeHostHeader", header = true, partName = "hostHeader",
                targetNamespace = "http://cixs.test.legstar.com/alltypes")
            LsfileaeHostHeader hostHeader)
        throws LsfileaeFault;
        
}
