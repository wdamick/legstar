
package com.legstar.test.cixs.lsfileal;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.legstar.test.coxb.lsfileal.ReplyDataType;

/**
 * Response wrapper element.
 * 
 * This class was generated by CIXS version 1.0.
 * 2007-02-06T14:31:05.015+01:00
 */

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsfilealResponse",
         namespace = "http://cixs.test.legstar.com/lsfileal",
         propOrder = {
    "response" 
})
public class LsfilealResponse {
	
    /** Inner JAXB-bound object. */
    @XmlElement(name = "Response",
                namespace = "http://cixs.test.legstar.com/lsfileal",
                required = true)
    private ReplyDataType response;

	/**
	 * Get the inner JAXB-bound object.
	 * 
	 * @return JAXB-bound object
	 */
	public final ReplyDataType getResponse() {
		return response;
	}

	/**
	 * Set the inner JAXB-bound object.
	 * 
	 * @param value JAXB-bound object
	 */
	public final void setResponse(
	    final ReplyDataType value) {
		this.response = value;
	}
}