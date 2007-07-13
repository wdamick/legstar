//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1.3-b01-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.07.13 at 09:00:27 AM CEST 
//


package com.legstar.test.coxb.dplarcht;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for LsSearchCriteriaType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsSearchCriteriaType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsStartwith">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;length value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LsStartwithLen">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;minInclusive value="0"/>
 *               &lt;maxInclusive value="999999999"/>
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
@XmlType(name = "LsSearchCriteriaType", propOrder = {
    "lsStartwith",
    "lsStartwithLen"
})
public class LsSearchCriteriaType {

    @XmlElement(name = "LsStartwith", required = true)
    @CobolElement(cobolName = "LS-STARTWITH", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 15, byteLength = 8, picture = "X(8)", usage = "DISPLAY", srceLine = 90)
    protected String lsStartwith;
    @XmlElement(name = "LsStartwithLen")
    @CobolElement(cobolName = "LS-STARTWITH-LEN", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 15, byteLength = 5, isSigned = false, totalDigits = 9, picture = "9(9)", usage = "PACKED-DECIMAL", srceLine = 91)
    protected long lsStartwithLen;

    /**
     * Gets the value of the lsStartwith property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsStartwith() {
        return lsStartwith;
    }

    /**
     * Sets the value of the lsStartwith property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsStartwith(String value) {
        this.lsStartwith = value;
    }

    public boolean isSetLsStartwith() {
        return (this.lsStartwith!= null);
    }

    /**
     * Gets the value of the lsStartwithLen property.
     * 
     */
    public long getLsStartwithLen() {
        return lsStartwithLen;
    }

    /**
     * Sets the value of the lsStartwithLen property.
     * 
     */
    public void setLsStartwithLen(long value) {
        this.lsStartwithLen = value;
    }

    public boolean isSetLsStartwithLen() {
        return true;
    }

}
