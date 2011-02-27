
package com.legstar.test.coxb.redbotha;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for Filler22 complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Filler22">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CLeftByte">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="1"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="CRightByte">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="1"/>
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
@XmlType(name = "Filler22", propOrder = {
    "cLeftByte",
    "cRightByte"
})
public class Filler22
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "CLeftByte", required = true)
    @CobolElement(cobolName = "C-LEFT-BYTE", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, picture = "X", srceLine = 23)
    protected String cLeftByte;
    @XmlElement(name = "CRightByte", required = true)
    @CobolElement(cobolName = "C-RIGHT-BYTE", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, picture = "X", srceLine = 24)
    protected String cRightByte;

    /**
     * Gets the value of the cLeftByte property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCLeftByte() {
        return cLeftByte;
    }

    /**
     * Sets the value of the cLeftByte property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCLeftByte(String value) {
        this.cLeftByte = value;
    }

    public boolean isSetCLeftByte() {
        return (this.cLeftByte!= null);
    }

    /**
     * Gets the value of the cRightByte property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCRightByte() {
        return cRightByte;
    }

    /**
     * Sets the value of the cRightByte property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCRightByte(String value) {
        this.cRightByte = value;
    }

    public boolean isSetCRightByte() {
        return (this.cRightByte!= null);
    }

}
