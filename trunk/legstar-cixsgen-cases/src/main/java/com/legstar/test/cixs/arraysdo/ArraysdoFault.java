
package com.legstar.test.cixs.arraysdo;
import javax.xml.ws.WebFault;

/**
 * Fault element used as return message when an exception occurs.
 * 
 * This class was generated by CIXS generator.
 * 2007-07-09T13:00:40.359+02:00
 */

@WebFault(name = "ArraysdoFaultInfo",
          targetNamespace = "http://cixs.test.legstar.com/arraysdo")
public class ArraysdoFault
    extends Exception {

    /** Default serialVersionUID.  */
	private static final long serialVersionUID = 1L;
	
    /** Java type that goes as soapenv:Fault detail element. */
    private ArraysdoFaultInfo faultInfo;

    /**
     * Constructor for Web Fault.
     * @param fault error details
     * @param message error summary
     */
    public ArraysdoFault(
    	final String message,
    	final ArraysdoFaultInfo fault) {
    	
        super(message);
        this.faultInfo = fault;
    }

    /**
     * Constructor for Web Fault with cause.
     * @param fault error details
     * @param message error summary
     * @param cause the cause
     */
    public ArraysdoFault(
        final String message,
        final ArraysdoFaultInfo fault,
        final Throwable cause) {
        
        super(message, cause);
        this.faultInfo = fault;
    }

    /**
     * 
     * @return
     *     returns fault bean: com.legstar.test.cixs.arraysdo.ArraysdoFaultInfo
     */
    public final ArraysdoFaultInfo getFaultInfo() {
        return faultInfo;
    }

}
