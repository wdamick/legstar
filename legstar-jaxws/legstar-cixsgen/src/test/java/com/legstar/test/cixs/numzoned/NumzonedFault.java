package com.legstar.test.cixs.numzoned;
import javax.xml.ws.WebFault;

/**
 * LegStar/Jaxws Operation fault.
 * Fault element used as return message when an exception occurs.
 * 
 * This class was generated by LegStar Mainframe Web Service adapter generator.
 */
@WebFault(name = "NumzonedFaultInfo",
          targetNamespace = "http://cixs.test.legstar.com/alltypes")
public class NumzonedFault
    extends Exception {

    /** Default serialVersionUID.  */
    private static final long serialVersionUID = 1L;

    /** Java type that goes as soapenv:Fault detail element. */
    private NumzonedFaultInfo faultInfo;

    /**
     * Constructor for Web Fault.
     * @param fault error details
     * @param message error summary
     */
    public NumzonedFault(
        final String message,
        final NumzonedFaultInfo fault) {
      
        super(message);
        faultInfo = fault;
    }

    /**
     * Constructor for Web Fault with cause.
     * @param fault error details
     * @param message error summary
     * @param cause the cause
     */
    public NumzonedFault(
        final String message,
        final NumzonedFaultInfo fault,
        final Throwable cause) {
        
        super(message, cause);
        faultInfo = fault;
    }

    /**
     * 
     * @return
     *     returns fault bean: com.legstar.test.cixs.numzoned.NumzonedFaultInfo
     */
    public NumzonedFaultInfo getFaultInfo() {
        return faultInfo;
    }

}

