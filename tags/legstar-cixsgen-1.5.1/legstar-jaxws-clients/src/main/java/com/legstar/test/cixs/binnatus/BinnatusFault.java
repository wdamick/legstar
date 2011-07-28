
package com.legstar.test.cixs.binnatus;

import javax.xml.ws.WebFault;


/**
 * This class was generated by the JAX-WS RI.
 * JAX-WS RI 2.1.3-b02-
 * Generated source version: 2.1
 * 
 */
@WebFault(name = "BinnatusFaultInfo", targetNamespace = "http://cixs.test.legstar.com/binnatus")
public class BinnatusFault
    extends Exception
{

    /**
     * Java type that goes as soapenv:Fault detail element.
     * 
     */
    private BinnatusFaultInfo faultInfo;

    /**
     * 
     * @param message
     * @param faultInfo
     */
    public BinnatusFault(String message, BinnatusFaultInfo faultInfo) {
        super(message);
        this.faultInfo = faultInfo;
    }

    /**
     * 
     * @param message
     * @param faultInfo
     * @param cause
     */
    public BinnatusFault(String message, BinnatusFaultInfo faultInfo, Throwable cause) {
        super(message, cause);
        this.faultInfo = faultInfo;
    }

    /**
     * 
     * @return
     *     returns fault bean: com.legstar.test.cixs.binnatus.BinnatusFaultInfo
     */
    public BinnatusFaultInfo getFaultInfo() {
        return faultInfo;
    }

}