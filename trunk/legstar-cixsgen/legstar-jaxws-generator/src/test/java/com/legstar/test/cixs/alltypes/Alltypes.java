package com.legstar.test.cixs.alltypes;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.ws.RequestWrapper;
import javax.xml.ws.ResponseWrapper;

import com.legstar.test.coxb.alltypes.Dfhcommarea;
/**
 * LegStar/Jaxws Component interface.
 * Each method maps to a CICS program. 
 * 
 * This class was generated by LegStar Mainframe Web Service adapter generator.
 */
@WebService(name = "alltypesPort",
            targetNamespace = "http://cixs.test.legstar.com/alltypes")
public interface Alltypes {
  
    /**
     * LegStar operation alltypes.
     * 
     * @param request a JAXB object mapping the request
     * @param hostHeader an object mapping header parameters
     * @return a JAXB object mapping the reply
     * @throws AlltypesFault if method fails
     */
    @WebMethod(operationName = "alltypes", action = "urn:alltypes")
    @WebResult(name = "Dfhcommarea",
        targetNamespace = "http://legstar.com/test/coxb/alltypes")
    @RequestWrapper(localName = "AlltypesRequest",
        targetNamespace = "http://cixs.test.legstar.com/alltypes",
        className = "com.legstar.test.cixs.alltypes.AlltypesRequest")
    @ResponseWrapper(localName = "AlltypesResponse",
        targetNamespace = "http://cixs.test.legstar.com/alltypes",
        className = "com.legstar.test.cixs.alltypes.AlltypesResponse")
    Dfhcommarea alltypes(
        @WebParam(name = "Dfhcommarea",
               targetNamespace = "http://legstar.com/test/coxb/alltypes")
            Dfhcommarea request,
        @WebParam(name = "AlltypesHostHeader", header = true, partName = "hostHeader",
                targetNamespace = "http://cixs.test.legstar.com/alltypes")
            AlltypesHostHeader hostHeader)
        throws AlltypesFault;
        
}