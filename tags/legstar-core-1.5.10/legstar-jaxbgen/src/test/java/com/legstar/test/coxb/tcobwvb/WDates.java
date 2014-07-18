
package com.legstar.test.coxb.tcobwvb;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WDates complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WDates">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Filler90">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler91">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler92">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler93">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler94">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
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
@XmlType(name = "WDates", propOrder = {
    "filler90",
    "filler91",
    "filler92",
    "filler93",
    "filler94"
})
public class WDates
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "Filler90", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(8)", value = "10/04/11", srceLine = 90)
    protected String filler90 = "10/04/11";
    @XmlElement(name = "Filler91", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(8)", value = "01/12/09", srceLine = 91)
    protected String filler91 = "01/12/09";
    @XmlElement(name = "Filler92", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(8)", value = "30/10/10", srceLine = 92)
    protected String filler92 = "30/10/10";
    @XmlElement(name = "Filler93", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(8)", value = "09/03/02", srceLine = 93)
    protected String filler93 = "09/03/02";
    @XmlElement(name = "Filler94", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(8)", value = "13/02/05", srceLine = 94)
    protected String filler94 = "13/02/05";

    /**
     * Gets the value of the filler90 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller90() {
        return filler90;
    }

    /**
     * Sets the value of the filler90 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller90(String value) {
        this.filler90 = value;
    }

    public boolean isSetFiller90() {
        return (this.filler90 != null);
    }

    /**
     * Gets the value of the filler91 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller91() {
        return filler91;
    }

    /**
     * Sets the value of the filler91 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller91(String value) {
        this.filler91 = value;
    }

    public boolean isSetFiller91() {
        return (this.filler91 != null);
    }

    /**
     * Gets the value of the filler92 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller92() {
        return filler92;
    }

    /**
     * Sets the value of the filler92 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller92(String value) {
        this.filler92 = value;
    }

    public boolean isSetFiller92() {
        return (this.filler92 != null);
    }

    /**
     * Gets the value of the filler93 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller93() {
        return filler93;
    }

    /**
     * Sets the value of the filler93 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller93(String value) {
        this.filler93 = value;
    }

    public boolean isSetFiller93() {
        return (this.filler93 != null);
    }

    /**
     * Gets the value of the filler94 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller94() {
        return filler94;
    }

    /**
     * Sets the value of the filler94 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller94(String value) {
        this.filler94 = value;
    }

    public boolean isSetFiller94() {
        return (this.filler94 != null);
    }

}
