
package com.legstar.test.coxb.redinout;

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
 *         &lt;element name="CNumeric" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="CBuffer" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="CParain" type="{http://legstar.com/test/coxb/redinout}CParainType" minOccurs="0"/>
 *         &lt;element name="CParaout" type="{http://legstar.com/test/coxb/redinout}CParaoutType" minOccurs="0"/>
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
    "cBuffer",
    "cParain",
    "cParaout"
})
public class DfhcommareaType {

    @XmlElement(name = "CNumeric")
    protected int cNumeric;
    @XmlElement(name = "CBuffer")
    protected String cBuffer;
    @XmlElement(name = "CParain")
    protected CParainType cParain;
    @XmlElement(name = "CParaout")
    protected CParaoutType cParaout;

    /**
     * Gets the value of the cNumeric property.
     * 
     */
    public int getCNumeric() {
        return cNumeric;
    }

    /**
     * Sets the value of the cNumeric property.
     * 
     */
    public void setCNumeric(int value) {
        this.cNumeric = value;
    }

    /**
     * Gets the value of the cBuffer property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCBuffer() {
        return cBuffer;
    }

    /**
     * Sets the value of the cBuffer property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCBuffer(String value) {
        this.cBuffer = value;
    }

    /**
     * Gets the value of the cParain property.
     * 
     * @return
     *     possible object is
     *     {@link CParainType }
     *     
     */
    public CParainType getCParain() {
        return cParain;
    }

    /**
     * Sets the value of the cParain property.
     * 
     * @param value
     *     allowed object is
     *     {@link CParainType }
     *     
     */
    public void setCParain(CParainType value) {
        this.cParain = value;
    }

    /**
     * Gets the value of the cParaout property.
     * 
     * @return
     *     possible object is
     *     {@link CParaoutType }
     *     
     */
    public CParaoutType getCParaout() {
        return cParaout;
    }

    /**
     * Sets the value of the cParaout property.
     * 
     * @param value
     *     allowed object is
     *     {@link CParaoutType }
     *     
     */
    public void setCParaout(CParaoutType value) {
        this.cParaout = value;
    }

}
