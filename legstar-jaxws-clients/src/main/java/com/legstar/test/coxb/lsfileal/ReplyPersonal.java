
package com.legstar.test.coxb.lsfileal;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ReplyPersonal complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ReplyPersonal">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ReplyName" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="ReplyAddress" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="ReplyPhone" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ReplyPersonal", propOrder = {
    "replyName",
    "replyAddress",
    "replyPhone"
})
public class ReplyPersonal {

    @XmlElement(name = "ReplyName", required = true)
    protected String replyName;
    @XmlElement(name = "ReplyAddress", required = true)
    protected String replyAddress;
    @XmlElement(name = "ReplyPhone", required = true)
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

}
