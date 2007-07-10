
package com.legstar.test.cixs.redbotha;
import javax.xml.ws.WebFault;

/**
 * Fault element used as return message when an exception occurs.
 * 
 * This class was generated by CIXS generator.
 * 2007-07-09T13:01:17.484+02:00
 */

@WebFault(name = "RedbothaFaultInfo",
          targetNamespace = "http://cixs.test.legstar.com/redbotha")
public class RedbothaFault
    extends Exception {

    /** Default serialVersionUID.  */
	private static final long serialVersionUID = 1L;
	
    /** Java type that goes as soapenv:Fault detail element. */
    private RedbothaFaultInfo faultInfo;

    /**
     * Constructor for Web Fault.
     * @param fault error details
     * @param message error summary
     */
    public RedbothaFault(
    	final String message,
    	final RedbothaFaultInfo fault) {
    	
        super(message);
        this.faultInfo = fault;
    }

    /**
     * Constructor for Web Fault with cause.
     * @param fault error details
     * @param message error summary
     * @param cause the cause
     */
    public RedbothaFault(
        final String message,
        final RedbothaFaultInfo fault,
        final Throwable cause) {
        
        super(message, cause);
        this.faultInfo = fault;
    }

    /**
     * 
     * @return
     *     returns fault bean: com.legstar.test.cixs.redbotha.RedbothaFaultInfo
     */
    public final RedbothaFaultInfo getFaultInfo() {
        return faultInfo;
    }

}
