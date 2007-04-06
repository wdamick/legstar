
package com.legstar.test.coxb.binnatsi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsUnsignedNativeType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsUnsignedNativeType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsHalfwords" type="{http://legstar.com/test/coxb/binnatsi}LsHalfwordsType"/>
 *         &lt;element name="LsFullwords" type="{http://legstar.com/test/coxb/binnatsi}LsFullwordsType"/>
 *         &lt;element name="LsDoublewords" type="{http://legstar.com/test/coxb/binnatsi}LsDoublewordsType"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsUnsignedNativeType", propOrder = {
    "lsHalfwords",
    "lsFullwords",
    "lsDoublewords"
})
public class LsUnsignedNativeType {

    @XmlElement(name = "LsHalfwords", required = true)
    protected LsHalfwordsType lsHalfwords;
    @XmlElement(name = "LsFullwords", required = true)
    protected LsFullwordsType lsFullwords;
    @XmlElement(name = "LsDoublewords", required = true)
    protected LsDoublewordsType lsDoublewords;

    /**
     * Gets the value of the lsHalfwords property.
     * 
     * @return
     *     possible object is
     *     {@link LsHalfwordsType }
     *     
     */
    public LsHalfwordsType getLsHalfwords() {
        return lsHalfwords;
    }

    /**
     * Sets the value of the lsHalfwords property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsHalfwordsType }
     *     
     */
    public void setLsHalfwords(LsHalfwordsType value) {
        this.lsHalfwords = value;
    }

    /**
     * Gets the value of the lsFullwords property.
     * 
     * @return
     *     possible object is
     *     {@link LsFullwordsType }
     *     
     */
    public LsFullwordsType getLsFullwords() {
        return lsFullwords;
    }

    /**
     * Sets the value of the lsFullwords property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsFullwordsType }
     *     
     */
    public void setLsFullwords(LsFullwordsType value) {
        this.lsFullwords = value;
    }

    /**
     * Gets the value of the lsDoublewords property.
     * 
     * @return
     *     possible object is
     *     {@link LsDoublewordsType }
     *     
     */
    public LsDoublewordsType getLsDoublewords() {
        return lsDoublewords;
    }

    /**
     * Sets the value of the lsDoublewords property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsDoublewordsType }
     *     
     */
    public void setLsDoublewords(LsDoublewordsType value) {
        this.lsDoublewords = value;
    }

}
