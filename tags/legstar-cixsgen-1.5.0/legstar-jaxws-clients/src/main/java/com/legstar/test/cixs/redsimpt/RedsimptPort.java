
package com.legstar.test.cixs.redsimpt;

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
@WebService(name = "redsimptPort", targetNamespace = "http://cixs.test.legstar.com/redsimpt")
@SOAPBinding(parameterStyle = SOAPBinding.ParameterStyle.BARE)
@XmlSeeAlso({
    com.legstar.test.coxb.redsimpt.ObjectFactory.class,
    com.legstar.test.cixs.redsimpt.ObjectFactory.class
})
public interface RedsimptPort {


    /**
     * 
     * @param hostHeader
     * @param parameters
     * @return
     *     returns com.legstar.test.cixs.redsimpt.RedsimptResponse
     * @throws RedsimptFault
     */
    @WebMethod(action = "urn:redsimpt")
    @WebResult(name = "RedsimptResponse", targetNamespace = "http://cixs.test.legstar.com/redsimpt", partName = "result")
    public RedsimptResponse redsimpt(
        @WebParam(name = "RedsimptRequest", targetNamespace = "http://cixs.test.legstar.com/redsimpt", partName = "parameters")
        RedsimptRequest parameters,
        @WebParam(name = "RedsimptHostHeader", targetNamespace = "http://cixs.test.legstar.com/redsimpt", header = true, partName = "hostHeader")
        RedsimptHostHeader hostHeader)
        throws RedsimptFault
    ;

}
