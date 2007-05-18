//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1.2-b01-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.05.14 at 05:10:38 PM CEST 
//


package com.legstar.test.lsfileac;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolType;
import com.legstar.coxb.annotation.CobolElement;


/**
 * <p>Java class for ReplyPersonalType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ReplyPersonalType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ReplyName">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;length value="20"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ReplyAddress">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;length value="20"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ReplyPhone">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;length value="8"/>
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
@XmlType(name = "ReplyPersonalType", propOrder = {
    "replyName",
    "replyAddress",
    "replyPhone"
})
public class ReplyPersonalType {

    @XmlElement(name = "ReplyName", required = true)
    @CobolElement(cobolName = "REPLY-NAME", type = CobolType.ALPHANUMERIC_ITEM, byteLength = 20, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected String replyName;
    @XmlElement(name = "ReplyAddress", required = true)
    @CobolElement(cobolName = "REPLY-ADDRESS", type = CobolType.ALPHANUMERIC_ITEM, byteLength = 20, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected String replyAddress;
    @XmlElement(name = "ReplyPhone", required = true)
    @CobolElement(cobolName = "REPLY-PHONE", type = CobolType.ALPHANUMERIC_ITEM, byteLength = 8, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected String replyPhone;

    /**
     * Gets the value of the replyName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReplyName() {
        return replyName;
    }

    /**
     * Sets the value of the replyName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReplyName(String value) {
        this.replyName = value;
    }

    public boolean isSetReplyName() {
        return (this.replyName!= null);
    }

    /**
     * Gets the value of the replyAddress property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReplyAddress() {
        return replyAddress;
    }

    /**
     * Sets the value of the replyAddress property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReplyAddress(String value) {
        this.replyAddress = value;
    }

    public boolean isSetReplyAddress() {
        return (this.replyAddress!= null);
    }

    /**
     * Gets the value of the replyPhone property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReplyPhone() {
        return replyPhone;
    }

    /**
     * Sets the value of the replyPhone property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReplyPhone(String value) {
        this.replyPhone = value;
    }

    public boolean isSetReplyPhone() {
        return (this.replyPhone!= null);
    }

}
