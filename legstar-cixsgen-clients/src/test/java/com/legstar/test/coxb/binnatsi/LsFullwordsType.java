
package com.legstar.test.coxb.binnatsi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsFullwordsType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsFullwordsType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsPs9X9Min" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="LsPs9X9Low" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="LsPs9X9High" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="LsPs9X9Max" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsFullwordsType", propOrder = {
    "lsPs9X9Min",
    "lsPs9X9Low",
    "lsPs9X9High",
    "lsPs9X9Max"
})
public class LsFullwordsType {

    @XmlElement(name = "LsPs9X9Min")
    protected int lsPs9X9Min;
    @XmlElement(name = "LsPs9X9Low")
    protected int lsPs9X9Low;
    @XmlElement(name = "LsPs9X9High")
    protected int lsPs9X9High;
    @XmlElement(name = "LsPs9X9Max")
    protected int lsPs9X9Max;

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
     * Gets the value of the lsPs9X9Low property.
     * 
     */
    public int getLsPs9X9Low() {
        return lsPs9X9Low;
    }

    /**
     * Sets the value of the lsPs9X9Low property.
     * 
     */
    public void setLsPs9X9Low(int value) {
        this.lsPs9X9Low = value;
    }

    /**
     * Gets the value of the lsPs9X9High property.
     * 
     */
    public int getLsPs9X9High() {
        return lsPs9X9High;
    }

    /**
     * Sets the value of the lsPs9X9High property.
     * 
     */
    public void setLsPs9X9High(int value) {
        this.lsPs9X9High = value;
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

}
