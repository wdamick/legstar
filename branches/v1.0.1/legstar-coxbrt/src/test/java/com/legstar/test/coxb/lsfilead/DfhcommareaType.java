//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1-b02-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.04.23 at 05:00:40 PM CEST 
//


package com.legstar.test.coxb.lsfilead;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolType;
import com.legstar.coxb.annotation.CobolElement;


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
 *         &lt;element name="ComNumber">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;minInclusive value="0"/>
 *               &lt;maxInclusive value="999999"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ComName">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;length value="20"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ComAddress">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;length value="20"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ComPhone">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;length value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ComDate">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;length value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ComAmount">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;length value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ComComment">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;length value="9"/>
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
@XmlType(name = "DfhcommareaType", propOrder = {
    "comNumber",
    "comName",
    "comAddress",
    "comPhone",
    "comDate",
    "comAmount",
    "comComment"
})
public class DfhcommareaType {

    @XmlElement(name = "ComNumber")
    @CobolElement(cobolName = "COM-NUMBER", type = CobolType.ZONED_DECIMAL_ITEM, byteLength = 6, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false, totalDigits = 6)
    protected long comNumber;
    @XmlElement(name = "ComName", required = true)
    @CobolElement(cobolName = "COM-NAME", type = CobolType.ALPHANUMERIC_ITEM, byteLength = 20, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected String comName;
    @XmlElement(name = "ComAddress", required = true)
    @CobolElement(cobolName = "COM-ADDRESS", type = CobolType.ALPHANUMERIC_ITEM, byteLength = 20, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected String comAddress;
    @XmlElement(name = "ComPhone", required = true)
    @CobolElement(cobolName = "COM-PHONE", type = CobolType.ALPHANUMERIC_ITEM, byteLength = 8, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected String comPhone;
    @XmlElement(name = "ComDate", required = true)
    @CobolElement(cobolName = "COM-DATE", type = CobolType.ALPHANUMERIC_ITEM, byteLength = 8, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected String comDate;
    @XmlElement(name = "ComAmount", required = true)
    @CobolElement(cobolName = "COM-AMOUNT", type = CobolType.ALPHANUMERIC_ITEM, byteLength = 8, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected String comAmount;
    @XmlElement(name = "ComComment", required = true)
    @CobolElement(cobolName = "COM-COMMENT", type = CobolType.ALPHANUMERIC_ITEM, byteLength = 9, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected String comComment;

    /**
     * Gets the value of the comNumber property.
     * 
     */
    public long getComNumber() {
        return comNumber;
    }

    /**
     * Sets the value of the comNumber property.
     * 
     */
    public void setComNumber(long value) {
        this.comNumber = value;
    }

    public boolean isSetComNumber() {
        return true;
    }

    /**
     * Gets the value of the comName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComName() {
        return comName;
    }

    /**
     * Sets the value of the comName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComName(String value) {
        this.comName = value;
    }

    public boolean isSetComName() {
        return (this.comName!= null);
    }

    /**
     * Gets the value of the comAddress property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComAddress() {
        return comAddress;
    }

    /**
     * Sets the value of the comAddress property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComAddress(String value) {
        this.comAddress = value;
    }

    public boolean isSetComAddress() {
        return (this.comAddress!= null);
    }

    /**
     * Gets the value of the comPhone property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComPhone() {
        return comPhone;
    }

    /**
     * Sets the value of the comPhone property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComPhone(String value) {
        this.comPhone = value;
    }

    public boolean isSetComPhone() {
        return (this.comPhone!= null);
    }

    /**
     * Gets the value of the comDate property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComDate() {
        return comDate;
    }

    /**
     * Sets the value of the comDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComDate(String value) {
        this.comDate = value;
    }

    public boolean isSetComDate() {
        return (this.comDate!= null);
    }

    /**
     * Gets the value of the comAmount property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComAmount() {
        return comAmount;
    }

    /**
     * Sets the value of the comAmount property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComAmount(String value) {
        this.comAmount = value;
    }

    public boolean isSetComAmount() {
        return (this.comAmount!= null);
    }

    /**
     * Gets the value of the comComment property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComComment() {
        return comComment;
    }

    /**
     * Sets the value of the comComment property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComComment(String value) {
        this.comComment = value;
    }

    public boolean isSetComComment() {
        return (this.comComment!= null);
    }

}
