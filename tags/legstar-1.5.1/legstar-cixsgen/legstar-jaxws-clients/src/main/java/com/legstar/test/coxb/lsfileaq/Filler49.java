
package com.legstar.test.coxb.lsfileaq;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for Filler49 complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Filler49">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LastTransDay" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="Filler51" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="LastTransMonth" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="Filler53" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="LastTransYear" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Filler49", propOrder = {
    "lastTransDay",
    "filler51",
    "lastTransMonth",
    "filler53",
    "lastTransYear"
})
public class Filler49 {

    @XmlElement(name = "LastTransDay", required = true)
    protected String lastTransDay;
    @XmlElement(name = "Filler51", required = true)
    protected String filler51;
    @XmlElement(name = "LastTransMonth", required = true)
    protected String lastTransMonth;
    @XmlElement(name = "Filler53", required = true)
    protected String filler53;
    @XmlElement(name = "LastTransYear", required = true)
    protected String lastTransYear;

    /**
     * Gets the value of the lastTransDay property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLastTransDay() {
        return lastTransDay;
    }

    /**
     * Sets the value of the lastTransDay property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLastTransDay(String value) {
        this.lastTransDay = value;
    }

    /**
     * Gets the value of the filler51 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller51() {
        return filler51;
    }

    /**
     * Sets the value of the filler51 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller51(String value) {
        this.filler51 = value;
    }

    /**
     * Gets the value of the lastTransMonth property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLastTransMonth() {
        return lastTransMonth;
    }

    /**
     * Sets the value of the lastTransMonth property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLastTransMonth(String value) {
        this.lastTransMonth = value;
    }

    /**
     * Gets the value of the filler53 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller53() {
        return filler53;
    }

    /**
     * Sets the value of the filler53 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller53(String value) {
        this.filler53 = value;
    }

    /**
     * Gets the value of the lastTransYear property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLastTransYear() {
        return lastTransYear;
    }

    /**
     * Sets the value of the lastTransYear property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLastTransYear(String value) {
        this.lastTransYear = value;
    }

}
