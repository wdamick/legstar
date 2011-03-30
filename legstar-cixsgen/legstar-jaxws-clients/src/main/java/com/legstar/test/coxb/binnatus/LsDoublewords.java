
package com.legstar.test.coxb.binnatus;

import java.math.BigInteger;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsDoublewords complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsDoublewords">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsP9X18Min" type="{http://www.w3.org/2001/XMLSchema}integer"/>
 *         &lt;element name="LsP9X18Low" type="{http://www.w3.org/2001/XMLSchema}integer"/>
 *         &lt;element name="LsP9X18High" type="{http://www.w3.org/2001/XMLSchema}integer"/>
 *         &lt;element name="LsP9X18Max" type="{http://www.w3.org/2001/XMLSchema}integer"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsDoublewords", propOrder = {
    "lsP9X18Min",
    "lsP9X18Low",
    "lsP9X18High",
    "lsP9X18Max"
})
public class LsDoublewords {

    @XmlElement(name = "LsP9X18Min", required = true)
    protected BigInteger lsP9X18Min;
    @XmlElement(name = "LsP9X18Low", required = true)
    protected BigInteger lsP9X18Low;
    @XmlElement(name = "LsP9X18High", required = true)
    protected BigInteger lsP9X18High;
    @XmlElement(name = "LsP9X18Max", required = true)
    protected BigInteger lsP9X18Max;

    /**
     * Gets the value of the lsP9X18Min property.
     * 
     * @return
     *     possible object is
     *     {@link BigInteger }
     *     
     */
    public BigInteger getLsP9X18Min() {
        return lsP9X18Min;
    }

    /**
     * Sets the value of the lsP9X18Min property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigInteger }
     *     
     */
    public void setLsP9X18Min(BigInteger value) {
        this.lsP9X18Min = value;
    }

    /**
     * Gets the value of the lsP9X18Low property.
     * 
     * @return
     *     possible object is
     *     {@link BigInteger }
     *     
     */
    public BigInteger getLsP9X18Low() {
        return lsP9X18Low;
    }

    /**
     * Sets the value of the lsP9X18Low property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigInteger }
     *     
     */
    public void setLsP9X18Low(BigInteger value) {
        this.lsP9X18Low = value;
    }

    /**
     * Gets the value of the lsP9X18High property.
     * 
     * @return
     *     possible object is
     *     {@link BigInteger }
     *     
     */
    public BigInteger getLsP9X18High() {
        return lsP9X18High;
    }

    /**
     * Sets the value of the lsP9X18High property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigInteger }
     *     
     */
    public void setLsP9X18High(BigInteger value) {
        this.lsP9X18High = value;
    }

    /**
     * Gets the value of the lsP9X18Max property.
     * 
     * @return
     *     possible object is
     *     {@link BigInteger }
     *     
     */
    public BigInteger getLsP9X18Max() {
        return lsP9X18Max;
    }

    /**
     * Sets the value of the lsP9X18Max property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigInteger }
     *     
     */
    public void setLsP9X18Max(BigInteger value) {
        this.lsP9X18Max = value;
    }

}
