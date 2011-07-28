
package com.legstar.test.cixs.lsfileal;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.test.coxb.lsfileal.RequestParms;


/**
 * <p>Java class for LsfilealRequest complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsfilealRequest">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{http://legstar.com/test/coxb/lsfileal}RequestParms"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsfilealRequest", propOrder = {
    "requestParms"
})
public class LsfilealRequest {

    @XmlElement(name = "RequestParms", namespace = "http://legstar.com/test/coxb/lsfileal", required = true)
    protected RequestParms requestParms;

    /**
     * Gets the value of the requestParms property.
     * 
     * @return
     *     possible object is
     *     {@link RequestParms }
     *     
     */
    public RequestParms getRequestParms() {
        return requestParms;
    }

    /**
     * Sets the value of the requestParms property.
     * 
     * @param value
     *     allowed object is
     *     {@link RequestParms }
     *     
     */
    public void setRequestParms(RequestParms value) {
        this.requestParms = value;
    }

}
