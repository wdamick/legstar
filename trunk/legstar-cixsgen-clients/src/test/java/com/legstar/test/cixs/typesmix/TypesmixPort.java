
package com.legstar.test.cixs.typesmix;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.jws.soap.SOAPBinding;
import javax.xml.bind.annotation.XmlSeeAlso;


/**
 * This class was generated by the JAXWS SI.
 * JAX-WS RI 2.1-01/26/2007 12:09 AM(kohsuke)-RC2
 * Generated source version: 2.1
 * 
 */
@WebService(name = "typesmixPort", targetNamespace = "http://cixs.test.legstar.com/typesmix")
@SOAPBinding(parameterStyle = SOAPBinding.ParameterStyle.BARE)
@XmlSeeAlso({
    com.legstar.test.coxb.typesmix.ObjectFactory.class,
    com.legstar.test.cixs.typesmix.ObjectFactory.class
})
public interface TypesmixPort {


    /**
     * 
     * @param hostHeader
     * @param parameters
     * @return
     *     returns com.legstar.test.cixs.typesmix.TypesmixResponse
     * @throws TypesmixFault
     */
    @WebMethod
    @WebResult(name = "TypesmixResponse", targetNamespace = "http://cixs.test.legstar.com/typesmix", partName = "result")
    public TypesmixResponse typesmix(
        @WebParam(name = "TypesmixRequest", targetNamespace = "http://cixs.test.legstar.com/typesmix", partName = "parameters")
        TypesmixRequest parameters,
        @WebParam(name = "HostHeader", targetNamespace = "http://cixs.test.legstar.com/typesmix", header = true, partName = "HostHeader")
        TypesmixHostHeader hostHeader)
        throws TypesmixFault
    ;

}
