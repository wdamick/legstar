
package com.legstar.test.coxb.lsfileal;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ReplyErrorHeaderType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ReplyErrorHeaderType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ReplyResp" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="ReplyResp2" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="ReplyMessage" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ReplyErrorHeaderType", propOrder = {
    "replyResp",
    "replyResp2",
    "replyMessage"
})
public class ReplyErrorHeaderType {

    @XmlElement(name = "ReplyResp")
    protected int replyResp;
    @XmlElement(name = "ReplyResp2")
    protected int replyResp2;
    @XmlElement(name = "ReplyMessage", required = true)
    protected String replyMessage;

    /**
     * Gets the value of the replyResp property.
     * 
     */
    public int getReplyResp() {
        return replyResp;
    }

    /**
     * Sets the value of the replyResp property.
     * 
     */
    public void setReplyResp(int value) {
        this.replyResp = value;
    }

    /**
     * Gets the value of the replyResp2 property.
     * 
     */
    public int getReplyResp2() {
        return replyResp2;
    }

    /**
     * Sets the value of the replyResp2 property.
     * 
     */
    public void setReplyResp2(int value) {
        this.replyResp2 = value;
    }

    /**
     * Gets the value of the replyMessage property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReplyMessage() {
        return replyMessage;
    }

    /**
     * Sets the value of the replyMessage property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReplyMessage(String value) {
        this.replyMessage = value;
    }

}
