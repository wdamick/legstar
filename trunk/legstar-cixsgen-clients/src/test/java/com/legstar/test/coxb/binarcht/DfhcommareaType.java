
package com.legstar.test.coxb.binarcht;

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
 *         &lt;element name="LsUnsignedNative" type="{http://legstar.com/test/coxb/binarcht}LsUnsignedNativeType"/>
 *         &lt;element name="LsSignedNative" type="{http://legstar.com/test/coxb/binarcht}LsSignedNativeType"/>
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
    "lsUnsignedNative",
    "lsSignedNative"
})
public class DfhcommareaType {

    @XmlElement(name = "LsUnsignedNative", required = true)
    protected LsUnsignedNativeType lsUnsignedNative;
    @XmlElement(name = "LsSignedNative", required = true)
    protected LsSignedNativeType lsSignedNative;

    /**
     * Gets the value of the lsUnsignedNative property.
     * 
     * @return
     *     possible object is
     *     {@link LsUnsignedNativeType }
     *     
     */
    public LsUnsignedNativeType getLsUnsignedNative() {
        return lsUnsignedNative;
    }

    /**
     * Sets the value of the lsUnsignedNative property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsUnsignedNativeType }
     *     
     */
    public void setLsUnsignedNative(LsUnsignedNativeType value) {
        this.lsUnsignedNative = value;
    }

    /**
     * Gets the value of the lsSignedNative property.
     * 
     * @return
     *     possible object is
     *     {@link LsSignedNativeType }
     *     
     */
    public LsSignedNativeType getLsSignedNative() {
        return lsSignedNative;
    }

    /**
     * Sets the value of the lsSignedNative property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsSignedNativeType }
     *     
     */
    public void setLsSignedNative(LsSignedNativeType value) {
        this.lsSignedNative = value;
    }

}
