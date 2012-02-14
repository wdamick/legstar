
package com.legstar.test.coxb.tcobwvb;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WLastNames complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WLastNames">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Filler66">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="15"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler67">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="15"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler68">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="15"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler69">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="15"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler70">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="15"/>
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
@XmlType(name = "WLastNames", propOrder = {
    "filler66",
    "filler67",
    "filler68",
    "filler69",
    "filler70"
})
public class WLastNames
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "Filler66", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(15)", value = "SMITH", srceLine = 66)
    protected String filler66 = "SMITH";
    @XmlElement(name = "Filler67", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(15)", value = "JOHNSON", srceLine = 67)
    protected String filler67 = "JOHNSON";
    @XmlElement(name = "Filler68", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(15)", value = "WILLIAMS", srceLine = 68)
    protected String filler68 = "WILLIAMS";
    @XmlElement(name = "Filler69", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(15)", value = "JONES", srceLine = 69)
    protected String filler69 = "JONES";
    @XmlElement(name = "Filler70", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(15)", value = "BROWN", srceLine = 70)
    protected String filler70 = "BROWN";

    /**
     * Gets the value of the filler66 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller66() {
        return filler66;
    }

    /**
     * Sets the value of the filler66 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller66(String value) {
        this.filler66 = value;
    }

    public boolean isSetFiller66() {
        return (this.filler66 != null);
    }

    /**
     * Gets the value of the filler67 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller67() {
        return filler67;
    }

    /**
     * Sets the value of the filler67 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller67(String value) {
        this.filler67 = value;
    }

    public boolean isSetFiller67() {
        return (this.filler67 != null);
    }

    /**
     * Gets the value of the filler68 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller68() {
        return filler68;
    }

    /**
     * Sets the value of the filler68 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller68(String value) {
        this.filler68 = value;
    }

    public boolean isSetFiller68() {
        return (this.filler68 != null);
    }

    /**
     * Gets the value of the filler69 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller69() {
        return filler69;
    }

    /**
     * Sets the value of the filler69 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller69(String value) {
        this.filler69 = value;
    }

    public boolean isSetFiller69() {
        return (this.filler69 != null);
    }

    /**
     * Gets the value of the filler70 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller70() {
        return filler70;
    }

    /**
     * Sets the value of the filler70 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller70(String value) {
        this.filler70 = value;
    }

    public boolean isSetFiller70() {
        return (this.filler70 != null);
    }

}
