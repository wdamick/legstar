
package com.legstar.test.coxb.binpkdus;

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
 *         &lt;element name="LsUnsignedPackedDecimal" type="{http://legstar.com/test/coxb/binpkdus}LsUnsignedPackedDecimalType"/>
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
    "lsUnsignedPackedDecimal"
})
public class DfhcommareaType {

    @XmlElement(name = "LsUnsignedPackedDecimal", required = true)
    protected LsUnsignedPackedDecimalType lsUnsignedPackedDecimal;

    /**
     * Gets the value of the lsUnsignedPackedDecimal property.
     * 
     * @return
     *     possible object is
     *     {@link LsUnsignedPackedDecimalType }
     *     
     */
    public LsUnsignedPackedDecimalType getLsUnsignedPackedDecimal() {
        return lsUnsignedPackedDecimal;
    }

    /**
     * Sets the value of the lsUnsignedPackedDecimal property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsUnsignedPackedDecimalType }
     *     
     */
    public void setLsUnsignedPackedDecimal(LsUnsignedPackedDecimalType value) {
        this.lsUnsignedPackedDecimal = value;
    }

}
