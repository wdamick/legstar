
package com.legstar.test.coxb.binpkdus;

import java.math.BigInteger;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsExtendType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsExtendType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsP9X19" type="{http://www.w3.org/2001/XMLSchema}integer"/>
 *         &lt;element name="LsP9X31" type="{http://www.w3.org/2001/XMLSchema}integer"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsExtendType", propOrder = {
    "lsP9X19",
    "lsP9X31"
})
public class LsExtendType {

    @XmlElement(name = "LsP9X19", required = true)
    protected BigInteger lsP9X19;
    @XmlElement(name = "LsP9X31", required = true)
    protected BigInteger lsP9X31;

    /**
     * Gets the value of the lsP9X19 property.
     * 
     * @return
     *     possible object is
     *     {@link BigInteger }
     *     
     */
    public BigInteger getLsP9X19() {
        return lsP9X19;
    }

    /**
     * Sets the value of the lsP9X19 property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigInteger }
     *     
     */
    public void setLsP9X19(BigInteger value) {
        this.lsP9X19 = value;
    }

    /**
     * Gets the value of the lsP9X31 property.
     * 
     * @return
     *     possible object is
     *     {@link BigInteger }
     *     
     */
    public BigInteger getLsP9X31() {
        return lsP9X31;
    }

    /**
     * Sets the value of the lsP9X31 property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigInteger }
     *     
     */
    public void setLsP9X31(BigInteger value) {
        this.lsP9X31 = value;
    }

}
