
package com.legstar.test.cixs.redsimpt;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.legstar.test.coxb.redsimpt.DfhcommareaType;

/**
 * Response wrapper element.
 * 
 * This class was generated by CIXS version 1.0.
 * 2007-02-06T14:30:58.984+01:00
 */

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "RedsimptResponse",
         namespace = "http://cixs.test.legstar.com/redsimpt",
         propOrder = {
    "response" 
})
public class RedsimptResponse {
	
    /** Inner JAXB-bound object. */
    @XmlElement(name = "Response",
                namespace = "http://cixs.test.legstar.com/redsimpt",
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