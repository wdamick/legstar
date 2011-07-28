
package com.legstar.test.coxb.redmulti;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


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
 *         &lt;element name="CErrorNum" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="CErrorDescription" type="{http://www.w3.org/2001/XMLSchema}string"/>
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
public class Filler38 {

    @XmlElement(name = "CErrorNum")
    protected int cErrorNum;
    @XmlElement(name = "CErrorDescription", required = true)
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

}
