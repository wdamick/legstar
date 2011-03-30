
package com.legstar.test.coxb.binnatus;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsHalfwords complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsHalfwords">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsP9X4Min" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="LsP9X4Low" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="LsP9X4High" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="LsP9X4Max" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsHalfwords", propOrder = {
    "lsP9X4Min",
    "lsP9X4Low",
    "lsP9X4High",
    "lsP9X4Max"
})
public class LsHalfwords {

    @XmlElement(name = "LsP9X4Min")
    protected int lsP9X4Min;
    @XmlElement(name = "LsP9X4Low")
    protected int lsP9X4Low;
    @XmlElement(name = "LsP9X4High")
    protected int lsP9X4High;
    @XmlElement(name = "LsP9X4Max")
    protected int lsP9X4Max;

    /**
     * Gets the value of the lsP9X4Min property.
     * 
     */
    public int getLsP9X4Min() {
        return lsP9X4Min;
    }

    /**
     * Sets the value of the lsP9X4Min property.
     * 
     */
    public void setLsP9X4Min(int value) {
        this.lsP9X4Min = value;
    }

    /**
     * Gets the value of the lsP9X4Low property.
     * 
     */
    public int getLsP9X4Low() {
        return lsP9X4Low;
    }

    /**
     * Sets the value of the lsP9X4Low property.
     * 
     */
    public void setLsP9X4Low(int value) {
        this.lsP9X4Low = value;
    }

    /**
     * Gets the value of the lsP9X4High property.
     * 
     */
    public int getLsP9X4High() {
        return lsP9X4High;
    }

    /**
     * Sets the value of the lsP9X4High property.
     * 
     */
    public void setLsP9X4High(int value) {
        this.lsP9X4High = value;
    }

    /**
     * Gets the value of the lsP9X4Max property.
     * 
     */
    public int getLsP9X4Max() {
        return lsP9X4Max;
    }

    /**
     * Sets the value of the lsP9X4Max property.
     * 
     */
    public void setLsP9X4Max(int value) {
        this.lsP9X4Max = value;
    }

}
