
package com.legstar.test.coxb.redmulti;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for Filler38 complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Filler38">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CErrorNum">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="4"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="CErrorDescription">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="196"/>
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
@XmlType(name = "Filler38", propOrder = {
    "cErrorNum",
    "cErrorDescription"
})
public class Filler38
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "CErrorNum")
    @CobolElement(cobolName = "C-ERROR-NUM", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 4, picture = "9(4)", srceLine = 39)
    protected int cErrorNum;
    @XmlElement(name = "CErrorDescription", required = true)
    @CobolElement(cobolName = "C-ERROR-DESCRIPTION", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(196)", srceLine = 40)
    protected String cErrorDescription;

    /**
     * Gets the value of the cErrorNum property.
     * 
     */
    public int getCErrorNum() {
        return cErrorNum;
    }

    /**
     * Sets the value of the cErrorNum property.
     * 
     */
    public void setCErrorNum(int value) {
        this.cErrorNum = value;
    }

    public boolean isSetCErrorNum() {
        return true;
    }

    /**
     * Gets the value of the cErrorDescription property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCErrorDescription() {
        return cErrorDescription;
    }

    /**
     * Sets the value of the cErrorDescription property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCErrorDescription(String value) {
        this.cErrorDescription = value;
    }

    public boolean isSetCErrorDescription() {
        return (this.cErrorDescription!= null);
    }

}
