
package com.legstar.test.coxb.binpkdus;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsUnsignedPackedDecimalType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsUnsignedPackedDecimalType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsCompat" type="{http://legstar.com/test/coxb/binpkdus}LsCompatType"/>
 *         &lt;element name="LsExtend" type="{http://legstar.com/test/coxb/binpkdus}LsExtendType"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsUnsignedPackedDecimalType", propOrder = {
    "lsCompat",
    "lsExtend"
})
public class LsUnsignedPackedDecimalType {

    @XmlElement(name = "LsCompat", required = true)
    protected LsCompatType lsCompat;
    @XmlElement(name = "LsExtend", required = true)
    protected LsExtendType lsExtend;

    /**
     * Gets the value of the lsCompat property.
     * 
     * @return
     *     possible object is
     *     {@link LsCompatType }
     *     
     */
    public LsCompatType getLsCompat() {
        return lsCompat;
    }

    /**
     * Sets the value of the lsCompat property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsCompatType }
     *     
     */
    public void setLsCompat(LsCompatType value) {
        this.lsCompat = value;
    }

    /**
     * Gets the value of the lsExtend property.
     * 
     * @return
     *     possible object is
     *     {@link LsExtendType }
     *     
     */
    public LsExtendType getLsExtend() {
        return lsExtend;
    }

    /**
     * Sets the value of the lsExtend property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsExtendType }
     *     
     */
    public void setLsExtend(LsExtendType value) {
        this.lsExtend = value;
    }

}
