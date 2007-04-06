
package com.legstar.test.coxb.redopera;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for Filler28Type complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Filler28Type">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CInteger" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="Filler30" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Filler28Type", propOrder = {
    "cInteger",
    "filler30"
})
public class Filler28Type {

    @XmlElement(name = "CInteger")
    protected int cInteger;
    @XmlElement(name = "Filler30", required = true)
    protected String filler30;

    /**
     * Gets the value of the cInteger property.
     * 
     */
    public int getCInteger() {
        return cInteger;
    }

    /**
     * Sets the value of the cInteger property.
     * 
     */
    public void setCInteger(int value) {
        this.cInteger = value;
    }

    /**
     * Gets the value of the filler30 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller30() {
        return filler30;
    }

    /**
     * Sets the value of the filler30 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller30(String value) {
        this.filler30 = value;
    }

}
