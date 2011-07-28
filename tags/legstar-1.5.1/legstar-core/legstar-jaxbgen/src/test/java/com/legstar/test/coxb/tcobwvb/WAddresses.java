
package com.legstar.test.coxb.tcobwvb;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WAddresses complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WAddresses">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Filler74">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="20"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler75">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="20"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler76">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="20"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler77">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="20"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler78">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="20"/>
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
@XmlType(name = "WAddresses", propOrder = {
    "filler74",
    "filler75",
    "filler76",
    "filler77",
    "filler78"
})
public class WAddresses
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "Filler74", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(20)", value = "CAMBRIDGE", srceLine = 74)
    protected String filler74 = "CAMBRIDGE";
    @XmlElement(name = "Filler75", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(20)", value = "BOSTON", srceLine = 75)
    protected String filler75 = "BOSTON";
    @XmlElement(name = "Filler76", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(20)", value = "NEW YORK", srceLine = 76)
    protected String filler76 = "NEW YORK";
    @XmlElement(name = "Filler77", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(20)", value = "SAN FRANCISCO", srceLine = 77)
    protected String filler77 = "SAN FRANCISCO";
    @XmlElement(name = "Filler78", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(20)", value = "SEATTLE", srceLine = 78)
    protected String filler78 = "SEATTLE";

    /**
     * Gets the value of the filler74 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller74() {
        return filler74;
    }

    /**
     * Sets the value of the filler74 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller74(String value) {
        this.filler74 = value;
    }

    public boolean isSetFiller74() {
        return (this.filler74 != null);
    }

    /**
     * Gets the value of the filler75 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller75() {
        return filler75;
    }

    /**
     * Sets the value of the filler75 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller75(String value) {
        this.filler75 = value;
    }

    public boolean isSetFiller75() {
        return (this.filler75 != null);
    }

    /**
     * Gets the value of the filler76 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller76() {
        return filler76;
    }

    /**
     * Sets the value of the filler76 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller76(String value) {
        this.filler76 = value;
    }

    public boolean isSetFiller76() {
        return (this.filler76 != null);
    }

    /**
     * Gets the value of the filler77 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller77() {
        return filler77;
    }

    /**
     * Sets the value of the filler77 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller77(String value) {
        this.filler77 = value;
    }

    public boolean isSetFiller77() {
        return (this.filler77 != null);
    }

    /**
     * Gets the value of the filler78 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller78() {
        return filler78;
    }

    /**
     * Sets the value of the filler78 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller78(String value) {
        this.filler78 = value;
    }

    public boolean isSetFiller78() {
        return (this.filler78 != null);
    }

}
