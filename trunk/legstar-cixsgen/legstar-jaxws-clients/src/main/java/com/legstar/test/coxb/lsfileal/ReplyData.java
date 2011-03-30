
package com.legstar.test.coxb.lsfileal;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ReplyData complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ReplyData">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ReplyType" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="ReplySuccessHeader" type="{http://legstar.com/test/coxb/lsfileal}ReplySuccessHeader" minOccurs="0"/>
 *         &lt;element name="ReplyErrorHeader" type="{http://legstar.com/test/coxb/lsfileal}ReplyErrorHeader" minOccurs="0"/>
 *         &lt;element name="Filler65" type="{http://legstar.com/test/coxb/lsfileal}Filler65"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ReplyData", propOrder = {
    "replyType",
    "replySuccessHeader",
    "replyErrorHeader",
    "filler65"
})
public class ReplyData {

    @XmlElement(name = "ReplyType")
    protected int replyType;
    @XmlElement(name = "ReplySuccessHeader")
    protected ReplySuccessHeader replySuccessHeader;
    @XmlElement(name = "ReplyErrorHeader")
    protected ReplyErrorHeader replyErrorHeader;
    @XmlElement(name = "Filler65", required = true)
    protected Filler65 filler65;

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
     *     {@link ReplySuccessHeader }
     *     
     */
    public ReplySuccessHeader getReplySuccessHeader() {
        return replySuccessHeader;
    }

    /**
     * Sets the value of the replySuccessHeader property.
     * 
     * @param value
     *     allowed object is
     *     {@link ReplySuccessHeader }
     *     
     */
    public void setReplySuccessHeader(ReplySuccessHeader value) {
        this.replySuccessHeader = value;
    }

    /**
     * Gets the value of the replyErrorHeader property.
     * 
     * @return
     *     possible object is
     *     {@link ReplyErrorHeader }
     *     
     */
    public ReplyErrorHeader getReplyErrorHeader() {
        return replyErrorHeader;
    }

    /**
     * Sets the value of the replyErrorHeader property.
     * 
     * @param value
     *     allowed object is
     *     {@link ReplyErrorHeader }
     *     
     */
    public void setReplyErrorHeader(ReplyErrorHeader value) {
        this.replyErrorHeader = value;
    }

    /**
     * Gets the value of the filler65 property.
     * 
     * @return
     *     possible object is
     *     {@link Filler65 }
     *     
     */
    public Filler65 getFiller65() {
        return filler65;
    }

    /**
     * Sets the value of the filler65 property.
     * 
     * @param value
     *     allowed object is
     *     {@link Filler65 }
     *     
     */
    public void setFiller65(Filler65 value) {
        this.filler65 = value;
    }

}
