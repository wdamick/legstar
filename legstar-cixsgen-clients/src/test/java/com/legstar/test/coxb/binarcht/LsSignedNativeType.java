
package com.legstar.test.coxb.binarcht;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsSignedNativeType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsSignedNativeType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsPs9X4Min" type="{http://www.w3.org/2001/XMLSchema}short"/>
 *         &lt;element name="LsPs9X4Max" type="{http://www.w3.org/2001/XMLSchema}short"/>
 *         &lt;element name="LsPs9X9Min" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="LsPs9X9Max" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="LsPs9X18Min" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="LsPs9X18Max" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsSignedNativeType", propOrder = {
    "lsPs9X4Min",
    "lsPs9X4Max",
    "lsPs9X9Min",
    "lsPs9X9Max",
    "lsPs9X18Min",
    "lsPs9X18Max"
})
public class LsSignedNativeType {

    @XmlElement(name = "LsPs9X4Min")
    protected short lsPs9X4Min;
    @XmlElement(name = "LsPs9X4Max")
    protected short lsPs9X4Max;
    @XmlElement(name = "LsPs9X9Min")
    protected int lsPs9X9Min;
    @XmlElement(name = "LsPs9X9Max")
    protected int lsPs9X9Max;
    @XmlElement(name = "LsPs9X18Min")
    protected long lsPs9X18Min;
    @XmlElement(name = "LsPs9X18Max")
    protected long lsPs9X18Max;

    /**
     * Gets the value of the lsPs9X4Min property.
     * 
     */
    public short getLsPs9X4Min() {
        return lsPs9X4Min;
    }

    /**
     * Sets the value of the lsPs9X4Min property.
     * 
     */
    public void setLsPs9X4Min(short value) {
        this.lsPs9X4Min = value;
    }

    /**
     * Gets the value of the lsPs9X4Max property.
     * 
     */
    public short getLsPs9X4Max() {
        return lsPs9X4Max;
    }

    /**
     * Sets the value of the lsPs9X4Max property.
     * 
     */
    public void setLsPs9X4Max(short value) {
        this.lsPs9X4Max = value;
    }

    /**
     * Gets the value of the lsPs9X9Min property.
     * 
     */
    public int getLsPs9X9Min() {
        return lsPs9X9Min;
    }

    /**
     * Sets the value of the lsPs9X9Min property.
     * 
     */
    public void setLsPs9X9Min(int value) {
        this.lsPs9X9Min = value;
    }

    /**
     * Gets the value of the lsPs9X9Max property.
     * 
     */
    public int getLsPs9X9Max() {
        return lsPs9X9Max;
    }

    /**
     * Sets the value of the lsPs9X9Max property.
     * 
     */
    public void setLsPs9X9Max(int value) {
        this.lsPs9X9Max = value;
    }

    /**
     * Gets the value of the lsPs9X18Min property.
     * 
     */
    public long getLsPs9X18Min() {
        return lsPs9X18Min;
    }

    /**
     * Sets the value of the lsPs9X18Min property.
     * 
     */
    public void setLsPs9X18Min(long value) {
        this.lsPs9X18Min = value;
    }

    /**
     * Gets the value of the lsPs9X18Max property.
     * 
     */
    public long getLsPs9X18Max() {
        return lsPs9X18Max;
    }

    /**
     * Sets the value of the lsPs9X18Max property.
     * 
     */
    public void setLsPs9X18Max(long value) {
        this.lsPs9X18Max = value;
    }

}
