//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1.3-b01-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.07.09 at 12:58:22 PM CEST 
//


package com.legstar.test.coxb.dplarcht;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for LsRequestType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsRequestType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsRequestType">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;minInclusive value="0"/>
 *               &lt;maxInclusive value="9999"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;choice>
 *           &lt;element name="LsAllItems">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *                 &lt;length value="4"/>
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
 *           &lt;element name="LsMaxItems">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *                 &lt;minInclusive value="0"/>
 *                 &lt;maxInclusive value="9999"/>
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
 *         &lt;/choice>
 *         &lt;element name="LsSearchCriteria" type="{http://legstar.com/test/coxb/dplarcht}LsSearchCriteriaType"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsRequestType", propOrder = {
    "lsRequestType",
    "lsAllItems",
    "lsMaxItems",
    "lsSearchCriteria"
})
public class LsRequestType {

    @XmlElement(name = "LsRequestType")
    @CobolElement(cobolName = "LS-REQUEST-TYPE", type = CobolType.BINARY_ITEM, levelNumber = 10, byteLength = 2, isSigned = false, totalDigits = 4, picture = "9(4)", usage = "BINARY", isCustomVariable = true, srceLine = 82)
    protected int lsRequestType;
    @XmlElement(name = "LsAllItems")
    @CobolElement(cobolName = "LS-ALL-ITEMS", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, byteLength = 4, isRedefined = true, picture = "X(4)", usage = "DISPLAY", srceLine = 86)
    protected String lsAllItems;
    @XmlElement(name = "LsMaxItems")
    @CobolElement(cobolName = "LS-MAX-ITEMS", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 10, byteLength = 4, isSigned = false, totalDigits = 4, redefines = "LS-ALL-ITEMS", picture = "9(4)", usage = "DISPLAY", srceLine = 88)
    protected Integer lsMaxItems;
    @XmlElement(name = "LsSearchCriteria", required = true)
    @CobolElement(cobolName = "LS-SEARCH-CRITERIA", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 89)
    protected LsSearchCriteriaType lsSearchCriteria;

    /**
     * Gets the value of the lsRequestType property.
     * 
     */
    public int getLsRequestType() {
        return lsRequestType;
    }

    /**
     * Sets the value of the lsRequestType property.
     * 
     */
    public void setLsRequestType(int value) {
        this.lsRequestType = value;
    }

    public boolean isSetLsRequestType() {
        return true;
    }

    /**
     * Gets the value of the lsAllItems property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsAllItems() {
        return lsAllItems;
    }

    /**
     * Sets the value of the lsAllItems property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsAllItems(String value) {
        this.lsAllItems = value;
    }

    public boolean isSetLsAllItems() {
        return (this.lsAllItems!= null);
    }

    /**
     * Gets the value of the lsMaxItems property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getLsMaxItems() {
        return lsMaxItems;
    }

    /**
     * Sets the value of the lsMaxItems property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setLsMaxItems(Integer value) {
        this.lsMaxItems = value;
    }

    public boolean isSetLsMaxItems() {
        return (this.lsMaxItems!= null);
    }

    /**
     * Gets the value of the lsSearchCriteria property.
     * 
     * @return
     *     possible object is
     *     {@link LsSearchCriteriaType }
     *     
     */
    public LsSearchCriteriaType getLsSearchCriteria() {
        return lsSearchCriteria;
    }

    /**
     * Sets the value of the lsSearchCriteria property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsSearchCriteriaType }
     *     
     */
    public void setLsSearchCriteria(LsSearchCriteriaType value) {
        this.lsSearchCriteria = value;
    }

    public boolean isSetLsSearchCriteria() {
        return (this.lsSearchCriteria!= null);
    }

}
