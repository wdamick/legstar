
package com.legstar.test.coxb.varar021;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for WellpointEaiEbsErrorRow complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WellpointEaiEbsErrorRow">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="WeerExceptionUuid" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "WellpointEaiEbsErrorRow", propOrder = {
    "weerExceptionUuid"
})
public class WellpointEaiEbsErrorRow {

    @XmlElement(name = "WeerExceptionUuid", required = true)
    protected String weerExceptionUuid;

    /**
     * Gets the value of the weerExceptionUuid property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getWeerExceptionUuid() {
        return weerExceptionUuid;
    }

    /**
     * Sets the value of the weerExceptionUuid property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setWeerExceptionUuid(String value) {
        this.weerExceptionUuid = value;
    }

}
