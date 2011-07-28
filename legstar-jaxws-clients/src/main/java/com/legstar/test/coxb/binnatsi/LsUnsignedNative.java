
package com.legstar.test.coxb.binnatsi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsUnsignedNative complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsUnsignedNative">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsHalfwords" type="{http://legstar.com/test/coxb/binnatsi}LsHalfwords"/>
 *         &lt;element name="LsFullwords" type="{http://legstar.com/test/coxb/binnatsi}LsFullwords"/>
 *         &lt;element name="LsDoublewords" type="{http://legstar.com/test/coxb/binnatsi}LsDoublewords"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsUnsignedNative", propOrder = {
    "lsHalfwords",
    "lsFullwords",
    "lsDoublewords"
})
public class LsUnsignedNative {

    @XmlElement(name = "LsHalfwords", required = true)
    protected LsHalfwords lsHalfwords;
    @XmlElement(name = "LsFullwords", required = true)
    protected LsFullwords lsFullwords;
    @XmlElement(name = "LsDoublewords", required = true)
    protected LsDoublewords lsDoublewords;

    /**
     * Gets the value of the lsHalfwords property.
     * 
     * @return
     *     possible object is
     *     {@link LsHalfwords }
     *     
     */
    public LsHalfwords getLsHalfwords() {
        return lsHalfwords;
    }

    /**
     * Sets the value of the lsHalfwords property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsHalfwords }
     *     
     */
    public void setLsHalfwords(LsHalfwords value) {
        this.lsHalfwords = value;
    }

    /**
     * Gets the value of the lsFullwords property.
     * 
     * @return
     *     possible object is
     *     {@link LsFullwords }
     *     
     */
    public LsFullwords getLsFullwords() {
        return lsFullwords;
    }

    /**
     * Sets the value of the lsFullwords property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsFullwords }
     *     
     */
    public void setLsFullwords(LsFullwords value) {
        this.lsFullwords = value;
    }

    /**
     * Gets the value of the lsDoublewords property.
     * 
     * @return
     *     possible object is
     *     {@link LsDoublewords }
     *     
     */
    public LsDoublewords getLsDoublewords() {
        return lsDoublewords;
    }

    /**
     * Sets the value of the lsDoublewords property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsDoublewords }
     *     
     */
    public void setLsDoublewords(LsDoublewords value) {
        this.lsDoublewords = value;
    }

}
