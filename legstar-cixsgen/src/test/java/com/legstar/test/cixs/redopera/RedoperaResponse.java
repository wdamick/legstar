
package com.legstar.test.cixs.redopera;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.legstar.test.coxb.redopera.DfhcommareaType;

/**
 * Response wrapper element.
 * 
 * This class was generated by CIXS version 1.0.
 * 2007-02-06T14:30:57.875+01:00
 */

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "RedoperaResponse",
         namespace = "http://cixs.test.legstar.com/redopera",
         propOrder = {
    "response" 
})
public class RedoperaResponse {
	
    /** Inner JAXB-bound object. */
    @XmlElement(name = "Response",
                namespace = "http://cixs.test.legstar.com/redopera",
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
