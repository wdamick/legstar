
package com.legstar.test.coxb.vararcom;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for CArrayType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CArrayType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CItem1" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="CItem2" type="{http://www.w3.org/2001/XMLSchema}short"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CArrayType", propOrder = {
    "cItem1",
    "cItem2"
})
public class CArrayType {

    @XmlElement(name = "CItem1", required = true)
    protected String cItem1;
    @XmlElement(name = "CItem2")
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

}
