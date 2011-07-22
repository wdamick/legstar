
package com.legstar.test.coxb.redbotha;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for Filler22 complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Filler22">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CLeftByte" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="CRightByte" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Filler22", propOrder = {
    "cLeftByte",
    "cRightByte"
})
public class Filler22 {

    @XmlElement(name = "CLeftByte", required = true)
    protected String cLeftByte;
    @XmlElement(name = "CRightByte", required = true)
    protected String cRightByte;

    /**
     * Gets the value of the cLeftByte property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCLeftByte() {
        return cLeftByte;
    }

    /**
     * Sets the value of the cLeftByte property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCLeftByte(String value) {
        this.cLeftByte = value;
    }

    /**
     * Gets the value of the cRightByte property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCRightByte() {
        return cRightByte;
    }

    /**
     * Sets the value of the cRightByte property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCRightByte(String value) {
        this.cRightByte = value;
    }

}
