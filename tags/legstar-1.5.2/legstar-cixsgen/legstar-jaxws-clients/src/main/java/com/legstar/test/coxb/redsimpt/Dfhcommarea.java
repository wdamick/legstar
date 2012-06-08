
package com.legstar.test.coxb.redsimpt;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for Dfhcommarea complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Dfhcommarea">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CDefinition1" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="CDefinition2" type="{http://www.w3.org/2001/XMLSchema}long" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Dfhcommarea", propOrder = {
    "cDefinition1",
    "cDefinition2"
})
public class Dfhcommarea {

    @XmlElement(name = "CDefinition1")
    protected String cDefinition1;
    @XmlElement(name = "CDefinition2")
    protected Long cDefinition2;

    /**
     * Gets the value of the cDefinition1 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCDefinition1() {
        return cDefinition1;
    }

    /**
     * Sets the value of the cDefinition1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCDefinition1(String value) {
        this.cDefinition1 = value;
    }

    /**
     * Gets the value of the cDefinition2 property.
     * 
     * @return
     *     possible object is
     *     {@link Long }
     *     
     */
    public Long getCDefinition2() {
        return cDefinition2;
    }

    /**
     * Sets the value of the cDefinition2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link Long }
     *     
     */
    public void setCDefinition2(Long value) {
        this.cDefinition2 = value;
    }

}
