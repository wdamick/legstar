
package com.legstar.test.coxb.redopera;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for Filler28 complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Filler28">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CInteger">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}int">
 *               &lt;totalDigits value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler30">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="192"/>
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
@XmlType(name = "Filler28", propOrder = {
    "cInteger",
    "filler30"
})
public class Filler28
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "CInteger")
    @CobolElement(cobolName = "C-INTEGER", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = true, totalDigits = 8, picture = "S9(8)", srceLine = 29)
    protected int cInteger;
    @XmlElement(name = "Filler30", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(192)", srceLine = 30)
    protected String filler30;

    /**
     * Gets the value of the cInteger property.
     * 
     */
    public int getCInteger() {
        return cInteger;
    }

    /**
     * Sets the value of the cInteger property.
     * 
     */
    public void setCInteger(int value) {
        this.cInteger = value;
    }

    public boolean isSetCInteger() {
        return true;
    }

    /**
     * Gets the value of the filler30 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller30() {
        return filler30;
    }

    /**
     * Sets the value of the filler30 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller30(String value) {
        this.filler30 = value;
    }

    public boolean isSetFiller30() {
        return (this.filler30 != null);
    }

}
