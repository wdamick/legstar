//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1.3-b01-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.07.17 at 10:43:33 AM CEST 
//


package com.legstar.test.coxb.redsimpt;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for DfhcommareaType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="DfhcommareaType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;choice>
 *           &lt;element name="CDefinition1">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *                 &lt;length value="18"/>
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
 *           &lt;element name="CDefinition2">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedLong">
 *                 &lt;minInclusive value="0"/>
 *                 &lt;maxInclusive value="999999999999999999"/>
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
 *         &lt;/choice>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DfhcommareaType", propOrder = {
    "cDefinition1",
    "cDefinition2"
})
public class DfhcommareaType {

    @XmlElement(name = "CDefinition1")
    @CobolElement(cobolName = "C-DEFINITION-1", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, byteLength = 18, isRedefined = true, picture = "X(18)", usage = "DISPLAY", unmarshalChoiceStrategyClassName = "com.legstar.coxb.cust.redsimpt.ChoiceSelector", srceLine = 21)
    protected String cDefinition1;
    @XmlElement(name = "CDefinition2")
    @CobolElement(cobolName = "C-DEFINITION-2", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, byteLength = 18, isSigned = false, totalDigits = 18, redefines = "C-DEFINITION-1", picture = "9(18)", usage = "DISPLAY", srceLine = 22)
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

    public boolean isSetCDefinition1() {
        return (this.cDefinition1 != null);
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

    public boolean isSetCDefinition2() {
        return (this.cDefinition2 != null);
    }

}
