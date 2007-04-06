
package com.legstar.test.coxb.lsfileal;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


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
 *         &lt;element name="ReplyType" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="ReplySuccessHeader" type="{http://legstar.com/test/coxb/lsfileal}ReplySuccessHeaderType" minOccurs="0"/>
 *         &lt;element name="ReplyErrorHeader" type="{http://legstar.com/test/coxb/lsfileal}ReplyErrorHeaderType" minOccurs="0"/>
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
    protected int replyType;
    @XmlElement(name = "ReplySuccessHeader")
    protected ReplySuccessHeaderType replySuccessHeader;
    @XmlElement(name = "ReplyErrorHeader")
    protected ReplyErrorHeaderType replyErrorHeader;
    @XmlElement(name = "Filler65", required = true)
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

}
