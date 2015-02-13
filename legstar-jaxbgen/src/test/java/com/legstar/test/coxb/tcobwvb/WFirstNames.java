
package com.legstar.test.coxb.tcobwvb;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WFirstNames complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WFirstNames">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Filler58">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler59">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler60">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler61">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler62">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
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
@XmlType(name = "WFirstNames", propOrder = {
    "filler58",
    "filler59",
    "filler60",
    "filler61",
    "filler62"
})
public class WFirstNames
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "Filler58", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(5)", value = "JOHN", srceLine = 58)
    protected String filler58 = "JOHN";
    @XmlElement(name = "Filler59", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(5)", value = "BILL", srceLine = 59)
    protected String filler59 = "BILL";
    @XmlElement(name = "Filler60", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(5)", value = "FRED", srceLine = 60)
    protected String filler60 = "FRED";
    @XmlElement(name = "Filler61", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(5)", value = "BOB", srceLine = 61)
    protected String filler61 = "BOB";
    @XmlElement(name = "Filler62", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(5)", value = "RORY", srceLine = 62)
    protected String filler62 = "RORY";

    /**
     * Gets the value of the filler58 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller58() {
        return filler58;
    }

    /**
     * Sets the value of the filler58 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller58(String value) {
        this.filler58 = value;
    }

    public boolean isSetFiller58() {
        return (this.filler58 != null);
    }

    /**
     * Gets the value of the filler59 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller59() {
        return filler59;
    }

    /**
     * Sets the value of the filler59 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller59(String value) {
        this.filler59 = value;
    }

    public boolean isSetFiller59() {
        return (this.filler59 != null);
    }

    /**
     * Gets the value of the filler60 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller60() {
        return filler60;
    }

    /**
     * Sets the value of the filler60 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller60(String value) {
        this.filler60 = value;
    }

    public boolean isSetFiller60() {
        return (this.filler60 != null);
    }

    /**
     * Gets the value of the filler61 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller61() {
        return filler61;
    }

    /**
     * Sets the value of the filler61 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller61(String value) {
        this.filler61 = value;
    }

    public boolean isSetFiller61() {
        return (this.filler61 != null);
    }

    /**
     * Gets the value of the filler62 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller62() {
        return filler62;
    }

    /**
     * Sets the value of the filler62 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller62(String value) {
        this.filler62 = value;
    }

    public boolean isSetFiller62() {
        return (this.filler62 != null);
    }

}
