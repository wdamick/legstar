
package com.legstar.test.cixs.arrayssm;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.legstar.test.coxb.arrayssm.DfhcommareaType;


/**
 * Response wrapper element.
 * 
 * This class was generated by CIXS generator.
 * 2007-07-09T13:00:42.562+02:00
 */

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ArrayssmResponse",
         namespace = "http://cixs.test.legstar.com/arrayssm",
         propOrder = {
    "response" 
})
public class ArrayssmResponse {
	
    /** Inner JAXB-bound object. */
    @XmlElement(name = "Response",
                namespace = "http://cixs.test.legstar.com/arrayssm",
                required = true)
    private DfhcommareaType response;

	/**
	 * Get the inner JAXB-bound object.
	 * 
	 * @return JAXB-bound object
	 */
	public final DfhcommareaType getResponse() {
		return response;
	}

	/**
	 * Set the inner JAXB-bound object.
	 * 
	 * @param value JAXB-bound object
	 */
	public final void setResponse(
	    final DfhcommareaType value) {
		this.response = value;
	}
}
