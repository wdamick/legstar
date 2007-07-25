//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1.3-b01-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.07.17 at 10:43:33 AM CEST 
//


package com.legstar.test.coxb.binnatus;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for LsUnsignedNativeType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsUnsignedNativeType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsHalfwords" type="{http://legstar.com/test/coxb/binnatus}LsHalfwordsType"/>
 *         &lt;element name="LsFullwords" type="{http://legstar.com/test/coxb/binnatus}LsFullwordsType"/>
 *         &lt;element name="LsDoublewords" type="{http://legstar.com/test/coxb/binnatus}LsDoublewordsType"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsUnsignedNativeType", propOrder = {
    "lsHalfwords",
    "lsFullwords",
    "lsDoublewords"
})
public class LsUnsignedNativeType {

    @XmlElement(name = "LsHalfwords", required = true)
    @CobolElement(cobolName = "LS-HALFWORDS", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 69)
    protected LsHalfwordsType lsHalfwords;
    @XmlElement(name = "LsFullwords", required = true)
    @CobolElement(cobolName = "LS-FULLWORDS", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 74)
    protected LsFullwordsType lsFullwords;
    @XmlElement(name = "LsDoublewords", required = true)
    @CobolElement(cobolName = "LS-DOUBLEWORDS", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 79)
    protected LsDoublewordsType lsDoublewords;

    /**
     * Gets the value of the lsHalfwords property.
     * 
     * @return
     *     possible object is
     *     {@link LsHalfwordsType }
     *     
     */
    public LsHalfwordsType getLsHalfwords() {
        return lsHalfwords;
    }

    /**
     * Sets the value of the lsHalfwords property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsHalfwordsType }
     *     
     */
    public void setLsHalfwords(LsHalfwordsType value) {
        this.lsHalfwords = value;
    }

    public boolean isSetLsHalfwords() {
        return (this.lsHalfwords!= null);
    }

    /**
     * Gets the value of the lsFullwords property.
     * 
     * @return
     *     possible object is
     *     {@link LsFullwordsType }
     *     
     */
    public LsFullwordsType getLsFullwords() {
        return lsFullwords;
    }

    /**
     * Sets the value of the lsFullwords property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsFullwordsType }
     *     
     */
    public void setLsFullwords(LsFullwordsType value) {
        this.lsFullwords = value;
    }

    public boolean isSetLsFullwords() {
        return (this.lsFullwords!= null);
    }

    /**
     * Gets the value of the lsDoublewords property.
     * 
     * @return
     *     possible object is
     *     {@link LsDoublewordsType }
     *     
     */
    public LsDoublewordsType getLsDoublewords() {
        return lsDoublewords;
    }

    /**
     * Sets the value of the lsDoublewords property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsDoublewordsType }
     *     
     */
    public void setLsDoublewords(LsDoublewordsType value) {
        this.lsDoublewords = value;
    }

    public boolean isSetLsDoublewords() {
        return (this.lsDoublewords!= null);
    }

}
