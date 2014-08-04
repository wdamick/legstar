
package com.legstar.test.coxb.redopera;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for Dfhcommarea complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Dfhcommarea">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CFunction" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="CData" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="Filler25" type="{http://legstar.com/test/coxb/redopera}Filler25" minOccurs="0"/>
 *         &lt;element name="Filler28" type="{http://legstar.com/test/coxb/redopera}Filler28" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Dfhcommarea", propOrder = {
    "cFunction",
    "cData",
    "filler25",
    "filler28"
})
public class Dfhcommarea {

    @XmlElement(name = "CFunction", required = true)
    protected String cFunction;
    @XmlElement(name = "CData")
    protected String cData;
    @XmlElement(name = "Filler25")
    protected Filler25 filler25;
    @XmlElement(name = "Filler28")
    protected Filler28 filler28;

    /**
     * Gets the value of the cFunction property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCFunction() {
        return cFunction;
    }

    /**
     * Sets the value of the cFunction property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCFunction(String value) {
        this.cFunction = value;
    }

    /**
     * Gets the value of the cData property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCData() {
        return cData;
    }

    /**
     * Sets the value of the cData property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCData(String value) {
        this.cData = value;
    }

    /**
     * Gets the value of the filler25 property.
     * 
     * @return
     *     possible object is
     *     {@link Filler25 }
     *     
     */
    public Filler25 getFiller25() {
        return filler25;
    }

    /**
     * Sets the value of the filler25 property.
     * 
     * @param value
     *     allowed object is
     *     {@link Filler25 }
     *     
     */
    public void setFiller25(Filler25 value) {
        this.filler25 = value;
    }

    /**
     * Gets the value of the filler28 property.
     * 
     * @return
     *     possible object is
     *     {@link Filler28 }
     *     
     */
    public Filler28 getFiller28() {
        return filler28;
    }

    /**
     * Sets the value of the filler28 property.
     * 
     * @param value
     *     allowed object is
     *     {@link Filler28 }
     *     
     */
    public void setFiller28(Filler28 value) {
        this.filler28 = value;
    }

}
