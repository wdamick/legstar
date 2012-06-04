
package com.legstar.test.cixs.dplarcht;

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
@WebService(name = "dplarchtPort", targetNamespace = "http://cixs.test.legstar.com/dplarcht")
@SOAPBinding(parameterStyle = SOAPBinding.ParameterStyle.BARE)
@XmlSeeAlso({
    com.legstar.test.cixs.dplarcht.ObjectFactory.class,
    com.legstar.test.coxb.dplarcht.ObjectFactory.class
})
public interface DplarchtPort {


    /**
     * 
     * @param hostHeader
     * @param parameters
     * @return
     *     returns com.legstar.test.cixs.dplarcht.DplarchtResponse
     * @throws DplarchtFault
     */
    @WebMethod(action = "urn:dplarcht")
    @WebResult(name = "DplarchtResponse", targetNamespace = "http://cixs.test.legstar.com/dplarcht", partName = "result")
    public DplarchtResponse dplarcht(
        @WebParam(name = "DplarchtRequest", targetNamespace = "http://cixs.test.legstar.com/dplarcht", partName = "parameters")
        DplarchtRequest parameters,
        @WebParam(name = "DplarchtHostHeader", targetNamespace = "http://cixs.test.legstar.com/dplarcht", header = true, partName = "hostHeader")
        DplarchtHostHeader hostHeader)
        throws DplarchtFault
    ;

}