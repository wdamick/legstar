
package com.legstar.test.coxb.redbotha;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for DfhcommareaType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="DfhcommareaType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CNumeric" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/>
 *         &lt;element name="Filler22" type="{http://legstar.com/test/coxb/redbotha}Filler22Type" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DfhcommareaType", propOrder = {
    "cNumeric",
    "filler22"
})
public class DfhcommareaType {

    @XmlElement(name = "CNumeric")
    protected Integer cNumeric;
    @XmlElement(name = "Filler22")
    protected Filler22Type filler22;

    /**
     * Gets the value of the cNumeric property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getCNumeric() {
        return cNumeric;
    }

    /**
     * Sets the value of the cNumeric property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setCNumeric(Integer value) {
        this.cNumeric = value;
    }

    /**
     * Gets the value of the filler22 property.
     * 
     * @return
     *     possible object is
     *     {@link Filler22Type }
     *     
     */
    public Filler22Type getFiller22() {
        return filler22;
    }

    /**
     * Sets the value of the filler22 property.
     * 
     * @param value
     *     allowed object is
     *     {@link Filler22Type }
     *     
     */
    public void setFiller22(Filler22Type value) {
        this.filler22 = value;
    }

}
