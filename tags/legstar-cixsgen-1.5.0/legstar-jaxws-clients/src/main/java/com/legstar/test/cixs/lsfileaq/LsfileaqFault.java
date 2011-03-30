
package com.legstar.test.cixs.lsfileaq;

import javax.xml.ws.WebFault;


/**
 * This class was generated by the JAX-WS RI.
 * JAX-WS RI 2.1.3-b02-
 * Generated source version: 2.1
 * 
 */
@WebFault(name = "LsfileaqFaultInfo", targetNamespace = "http://cixs.test.legstar.com/lsfileaq")
public class LsfileaqFault
    extends Exception
{

    /**
     * Java type that goes as soapenv:Fault detail element.
     * 
     */
    private LsfileaqFaultInfo faultInfo;

    /**
     * 
     * @param message
     * @param faultInfo
     */
    public LsfileaqFault(String message, LsfileaqFaultInfo faultInfo) {
        super(message);
        this.faultInfo = faultInfo;
    }

    /**
     * 
     * @param message
     * @param faultInfo
     * @param cause
     */
    public LsfileaqFault(String message, LsfileaqFaultInfo faultInfo, Throwable cause) {
        super(message, cause);
        this.faultInfo = faultInfo;
    }

    /**
     * 
     * @return
     *     returns fault bean: com.legstar.test.cixs.lsfileaq.LsfileaqFaultInfo
     */
    public LsfileaqFaultInfo getFaultInfo() {
        return faultInfo;
    }

}
