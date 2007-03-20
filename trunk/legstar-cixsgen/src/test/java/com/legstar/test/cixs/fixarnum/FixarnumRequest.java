
package com.legstar.test.cixs.fixarnum;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.legstar.test.coxb.fixarnum.DfhcommareaType;

/**
 * Request wrapper element.
 * 
 * This class was generated by CIXS version 1.0.
 * 2007-02-06T14:30:45.203+01:00
 */

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FixarnumRequest",
         namespace = "http://cixs.test.legstar.com/fixarnum",
         propOrder = {
    "request" 
})
public class FixarnumRequest {
	
    /** Inner JAXB-bound object. */
    @XmlElement(name = "Request",
                namespace = "http://cixs.test.legstar.com/fixarnum",
                required = true)
    private DfhcommareaType request;

	/**
	 * Get the inner JAXB-bound object.
	 * 
	 * @return JAXB-bound object
	 */
	public final DfhcommareaType getRequest() {
		return request;
	}

	/**
	 * Set the inner JAXB-bound object.
	 * 
	 * @param value JAXB-bound object
	 */
	public final void setRequest(
	    final DfhcommareaType value) {
		this.request = value;
	}
}
