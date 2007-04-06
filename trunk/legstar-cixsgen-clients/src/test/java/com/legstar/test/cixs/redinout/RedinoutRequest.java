
package com.legstar.test.cixs.redinout;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.test.coxb.redinout.DfhcommareaType;


/**
 * <p>Java class for RedinoutRequest complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="RedinoutRequest">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Request" type="{http://legstar.com/test/coxb/redinout}DfhcommareaType"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "RedinoutRequest", propOrder = {
    "request"
})
public class RedinoutRequest {

    @XmlElement(name = "Request", required = true)
    protected DfhcommareaType request;

    /**
     * Gets the value of the request property.
     * 
     * @return
     *     possible object is
     *     {@link DfhcommareaType }
     *     
     */
    public DfhcommareaType getRequest() {
        return request;
    }

    /**
     * Sets the value of the request property.
     * 
     * @param value
     *     allowed object is
     *     {@link DfhcommareaType }
     *     
     */
    public void setRequest(DfhcommareaType value) {
        this.request = value;
    }

}
