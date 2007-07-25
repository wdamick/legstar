//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1.3-b01-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.07.17 at 10:43:33 AM CEST 
//


package com.legstar.test.coxb.lsfileal;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for ReplyDataType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ReplyDataType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ReplyType">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;minInclusive value="0"/>
 *               &lt;maxInclusive value="9999"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;choice>
 *           &lt;element name="ReplySuccessHeader" type="{http://legstar.com/test/coxb/lsfileal}ReplySuccessHeaderType"/>
 *           &lt;element name="ReplyErrorHeader" type="{http://legstar.com/test/coxb/lsfileal}ReplyErrorHeaderType"/>
 *         &lt;/choice>
 *         &lt;element name="Filler65" type="{http://legstar.com/test/coxb/lsfileal}Filler65Type"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ReplyDataType", propOrder = {
    "replyType",
    "replySuccessHeader",
    "replyErrorHeader",
    "filler65"
})
public class ReplyDataType {

    @XmlElement(name = "ReplyType")
    @CobolElement(cobolName = "REPLY-TYPE", type = CobolType.BINARY_ITEM, levelNumber = 5, byteLength = 2, isSigned = false, totalDigits = 4, picture = "9(4)", usage = "BINARY", srceLine = 54)
    protected int replyType;
    @XmlElement(name = "ReplySuccessHeader")
    @CobolElement(cobolName = "REPLY-SUCCESS-HEADER", type = CobolType.GROUP_ITEM, levelNumber = 5, isRedefined = true, srceLine = 57)
    protected ReplySuccessHeaderType replySuccessHeader;
    @XmlElement(name = "ReplyErrorHeader")
    @CobolElement(cobolName = "REPLY-ERROR-HEADER", type = CobolType.GROUP_ITEM, levelNumber = 5, redefines = "REPLY-SUCCESS-HEADER", srceLine = 61)
    protected ReplyErrorHeaderType replyErrorHeader;
    @XmlElement(name = "Filler65", required = true)
    @CobolElement(cobolName = "FILLER-65", type = CobolType.GROUP_ITEM, levelNumber = 5, srceLine = 65)
    protected Filler65Type filler65;

    /**
     * Gets the value of the replyType property.
     * 
     */
    public int getReplyType() {
        return replyType;
    }

    /**
     * Sets the value of the replyType property.
     * 
     */
    public void setReplyType(int value) {
        this.replyType = value;
    }

    public boolean isSetReplyType() {
        return true;
    }

    /**
     * Gets the value of the replySuccessHeader property.
     * 
     * @return
     *     possible object is
     *     {@link ReplySuccessHeaderType }
     *     
     */
    public ReplySuccessHeaderType getReplySuccessHeader() {
        return replySuccessHeader;
    }

    /**
     * Sets the value of the replySuccessHeader property.
     * 
     * @param value
     *     allowed object is
     *     {@link ReplySuccessHeaderType }
     *     
     */
    public void setReplySuccessHeader(ReplySuccessHeaderType value) {
        this.replySuccessHeader = value;
    }

    public boolean isSetReplySuccessHeader() {
        return (this.replySuccessHeader!= null);
    }

    /**
     * Gets the value of the replyErrorHeader property.
     * 
     * @return
     *     possible object is
     *     {@link ReplyErrorHeaderType }
     *     
     */
    public ReplyErrorHeaderType getReplyErrorHeader() {
        return replyErrorHeader;
    }

    /**
     * Sets the value of the replyErrorHeader property.
     * 
     * @param value
     *     allowed object is
     *     {@link ReplyErrorHeaderType }
     *     
     */
    public void setReplyErrorHeader(ReplyErrorHeaderType value) {
        this.replyErrorHeader = value;
    }

    public boolean isSetReplyErrorHeader() {
        return (this.replyErrorHeader!= null);
    }

    /**
     * Gets the value of the filler65 property.
     * 
     * @return
     *     possible object is
     *     {@link Filler65Type }
     *     
     */
    public Filler65Type getFiller65() {
        return filler65;
    }

    /**
     * Sets the value of the filler65 property.
     * 
     * @param value
     *     allowed object is
     *     {@link Filler65Type }
     *     
     */
    public void setFiller65(Filler65Type value) {
        this.filler65 = value;
    }

    public boolean isSetFiller65() {
        return (this.filler65 != null);
    }

}
