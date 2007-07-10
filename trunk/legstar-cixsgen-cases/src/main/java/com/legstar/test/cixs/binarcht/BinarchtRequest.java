
package com.legstar.test.cixs.binarcht;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.legstar.test.coxb.binarcht.DfhcommareaType;


/**
 * Request wrapper element.
 * 
 * This class was generated by CIXS generator.
 * 2007-07-09T13:00:44.812+02:00
 */

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "BinarchtRequest",
         namespace = "http://cixs.test.legstar.com/binarcht",
         propOrder = {
    "request" 
})
public class BinarchtRequest {
	
    /** Inner JAXB-bound object. */
    @XmlElement(name = "Request",
                namespace = "http://cixs.test.legstar.com/binarcht",
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
