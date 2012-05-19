
package com.legstar.test.coxb.lsfileaq;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


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
 *         &lt;element name="LastTransDay">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="2"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler51">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="1"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LastTransMonth">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="2"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler53">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="1"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LastTransYear">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="2"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
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
public class Filler49
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "LastTransDay", required = true)
    @CobolElement(cobolName = "LAST-TRANS-DAY", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X(2)", srceLine = 50)
    protected String lastTransDay;
    @XmlElement(name = "Filler51", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X", srceLine = 51)
    protected String filler51;
    @XmlElement(name = "LastTransMonth", required = true)
    @CobolElement(cobolName = "LAST-TRANS-MONTH", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X(2)", srceLine = 52)
    protected String lastTransMonth;
    @XmlElement(name = "Filler53", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X", srceLine = 53)
    protected String filler53;
    @XmlElement(name = "LastTransYear", required = true)
    @CobolElement(cobolName = "LAST-TRANS-YEAR", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X(2)", srceLine = 54)
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

    public boolean isSetLastTransDay() {
        return (this.lastTransDay!= null);
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

    public boolean isSetFiller51() {
        return (this.filler51 != null);
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

    public boolean isSetLastTransMonth() {
        return (this.lastTransMonth!= null);
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

    public boolean isSetFiller53() {
        return (this.filler53 != null);
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

    public boolean isSetLastTransYear() {
        return (this.lastTransYear!= null);
    }

}
