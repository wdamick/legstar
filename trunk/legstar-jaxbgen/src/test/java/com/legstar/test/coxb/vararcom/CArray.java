
package com.legstar.test.coxb.vararcom;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for CArray complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CArray">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CItem1">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="CItem2">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}short">
 *               &lt;totalDigits value="4"/>
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
@XmlType(name = "CArray", propOrder = {
    "cItem1",
    "cItem2"
})
public class CArray
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "CItem1", required = true)
    @CobolElement(cobolName = "C-ITEM-1", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, picture = "X(5)", srceLine = 24)
    protected String cItem1;
    @XmlElement(name = "CItem2")
    @CobolElement(cobolName = "C-ITEM-2", type = CobolType.BINARY_ITEM, levelNumber = 10, isSigned = true, totalDigits = 4, picture = "S9(4)", usage = "BINARY", srceLine = 25)
    protected short cItem2;

    /**
     * Gets the value of the cItem1 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCItem1() {
        return cItem1;
    }

    /**
     * Sets the value of the cItem1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCItem1(String value) {
        this.cItem1 = value;
    }

    public boolean isSetCItem1() {
        return (this.cItem1 != null);
    }

    /**
     * Gets the value of the cItem2 property.
     * 
     */
    public short getCItem2() {
        return cItem2;
    }

    /**
     * Sets the value of the cItem2 property.
     * 
     */
    public void setCItem2(short value) {
        this.cItem2 = value;
    }

    public boolean isSetCItem2() {
        return true;
    }

}
