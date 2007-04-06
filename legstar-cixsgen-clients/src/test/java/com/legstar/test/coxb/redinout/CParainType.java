
package com.legstar.test.coxb.redinout;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for CParainType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CParainType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CSomeInput" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CParainType", propOrder = {
    "cSomeInput"
})
public class CParainType {

    @XmlElement(name = "CSomeInput", required = true)
    protected String cSomeInput;

    /**
     * Gets the value of the cSomeInput property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCSomeInput() {
        return cSomeInput;
    }

    /**
     * Sets the value of the cSomeInput property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCSomeInput(String value) {
        this.cSomeInput = value;
    }

}
