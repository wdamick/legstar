
package com.legstar.test.cixs.typesmix;
import javax.xml.ws.WebFault;

/**
 * Fault element used as return message when an exception occurs.
 * 
 * This class was generated by CIXS generator.
 * 2007-07-09T13:01:28.046+02:00
 */

@WebFault(name = "TypesmixFaultInfo",
          targetNamespace = "http://cixs.test.legstar.com/typesmix")
public class TypesmixFault
    extends Exception {

    /** Default serialVersionUID.  */
	private static final long serialVersionUID = 1L;
	
    /** Java type that goes as soapenv:Fault detail element. */
    private TypesmixFaultInfo faultInfo;

    /**
     * Constructor for Web Fault.
     * @param fault error details
     * @param message error summary
     */
    public TypesmixFault(
    	final String message,
    	final TypesmixFaultInfo fault) {
    	
        super(message);
        this.faultInfo = fault;
    }

    /**
     * Constructor for Web Fault with cause.
     * @param fault error details
     * @param message error summary
     * @param cause the cause
     */
    public TypesmixFault(
        final String message,
        final TypesmixFaultInfo fault,
        final Throwable cause) {
        
        super(message, cause);
        this.faultInfo = fault;
    }

    /**
     * 
     * @return
     *     returns fault bean: com.legstar.test.cixs.typesmix.TypesmixFaultInfo
     */
    public final TypesmixFaultInfo getFaultInfo() {
        return faultInfo;
    }

}
