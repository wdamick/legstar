
package com.legstar.test.coxb.tcobwvb;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WPhones complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WPhones">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Filler82">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler83">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler84">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler85">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler86">
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
@XmlType(name = "WPhones", propOrder = {
    "filler82",
    "filler83",
    "filler84",
    "filler85",
    "filler86"
})
public class WPhones
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "Filler82", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(8)", value = "25663488", srceLine = 82)
    protected String filler82 = "25663488";
    @XmlElement(name = "Filler83", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(8)", value = "38791206", srceLine = 83)
    protected String filler83 = "38791206";
    @XmlElement(name = "Filler84", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(8)", value = "67159589", srceLine = 84)
    protected String filler84 = "67159589";
    @XmlElement(name = "Filler85", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(8)", value = "54845428", srceLine = 85)
    protected String filler85 = "54845428";
    @XmlElement(name = "Filler86", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(8)", value = "48952235", srceLine = 86)
    protected String filler86 = "48952235";

    /**
     * Gets the value of the filler82 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller82() {
        return filler82;
    }

    /**
     * Sets the value of the filler82 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller82(String value) {
        this.filler82 = value;
    }

    public boolean isSetFiller82() {
        return (this.filler82 != null);
    }

    /**
     * Gets the value of the filler83 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller83() {
        return filler83;
    }

    /**
     * Sets the value of the filler83 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller83(String value) {
        this.filler83 = value;
    }

    public boolean isSetFiller83() {
        return (this.filler83 != null);
    }

    /**
     * Gets the value of the filler84 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller84() {
        return filler84;
    }

    /**
     * Sets the value of the filler84 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller84(String value) {
        this.filler84 = value;
    }

    public boolean isSetFiller84() {
        return (this.filler84 != null);
    }

    /**
     * Gets the value of the filler85 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller85() {
        return filler85;
    }

    /**
     * Sets the value of the filler85 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller85(String value) {
        this.filler85 = value;
    }

    public boolean isSetFiller85() {
        return (this.filler85 != null);
    }

    /**
     * Gets the value of the filler86 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller86() {
        return filler86;
    }

    /**
     * Sets the value of the filler86 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller86(String value) {
        this.filler86 = value;
    }

    public boolean isSetFiller86() {
        return (this.filler86 != null);
    }

}
