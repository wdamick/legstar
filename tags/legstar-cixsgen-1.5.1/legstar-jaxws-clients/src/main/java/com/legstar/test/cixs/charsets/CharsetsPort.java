
package com.legstar.test.cixs.charsets;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.jws.soap.SOAPBinding;
import javax.xml.bind.annotation.XmlSeeAlso;


/**
 * This class was generated by the JAX-WS RI.
 * JAX-WS RI 2.1.3-b02-
 * Generated source version: 2.1
 * 
 */
@WebService(name = "charsetsPort", targetNamespace = "http://cixs.test.legstar.com/charsets")
@SOAPBinding(parameterStyle = SOAPBinding.ParameterStyle.BARE)
@XmlSeeAlso({
    com.legstar.test.coxb.charsets.ObjectFactory.class,
    com.legstar.test.cixs.charsets.ObjectFactory.class
})
public interface CharsetsPort {


    /**
     * 
     * @param hostHeader
     * @param parameters
     * @return
     *     returns com.legstar.test.cixs.charsets.CharsetsResponse
     * @throws CharsetsFault
     */
    @WebMethod(action = "urn:charsets")
    @WebResult(name = "CharsetsResponse", targetNamespace = "http://cixs.test.legstar.com/charsets", partName = "result")
    public CharsetsResponse charsets(
        @WebParam(name = "CharsetsRequest", targetNamespace = "http://cixs.test.legstar.com/charsets", partName = "parameters")
        CharsetsRequest parameters,
        @WebParam(name = "CharsetsHostHeader", targetNamespace = "http://cixs.test.legstar.com/charsets", header = true, partName = "hostHeader")
        CharsetsHostHeader hostHeader)
        throws CharsetsFault
    ;

}
