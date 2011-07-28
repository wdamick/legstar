package com.legstar.test.cixs.fixarnum;
import javax.xml.ws.WebFault;

/**
 * LegStar/Jaxws Operation fault.
 * Fault element used as return message when an exception occurs.
 * 
 * This class was generated by LegStar Mainframe Web Service adapter generator.
 */
@WebFault(name = "FixarnumFaultInfo",
          targetNamespace = "http://cixs.test.legstar.com/fixarnum")
public class FixarnumFault
    extends Exception {

    /** Default serialVersionUID.  */
    private static final long serialVersionUID = 1L;

    /** Java type that goes as soapenv:Fault detail element. */
    private FixarnumFaultInfo faultInfo;

    /**
     * Constructor for Web Fault.
     * @param fault error details
     * @param message error summary
     */
    public FixarnumFault(
        final String message,
        final FixarnumFaultInfo fault) {
      
        super(message);
        faultInfo = fault;
    }

    /**
     * Constructor for Web Fault with cause.
     * @param fault error details
     * @param message error summary
     * @param cause the cause
     */
    public FixarnumFault(
        final String message,
        final FixarnumFaultInfo fault,
        final Throwable cause) {
        
        super(message, cause);
        faultInfo = fault;
    }

    /**
     * 
     * @return
     *     returns fault bean: com.legstar.test.cixs.fixarnum.FixarnumFaultInfo
     */
    public FixarnumFaultInfo getFaultInfo() {
        return faultInfo;
    }

}

