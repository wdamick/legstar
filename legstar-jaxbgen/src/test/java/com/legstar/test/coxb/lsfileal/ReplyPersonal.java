
package com.legstar.test.coxb.lsfileal;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


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
 *         &lt;element name="ReplyName">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="20"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ReplyAddress">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="20"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ReplyPhone">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
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
@XmlType(name = "ReplyPersonal", propOrder = {
    "replyName",
    "replyAddress",
    "replyPhone"
})
public class ReplyPersonal
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "ReplyName", required = true)
    @CobolElement(cobolName = "REPLY-NAME", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X(20)", srceLine = 71)
    protected String replyName;
    @XmlElement(name = "ReplyAddress", required = true)
    @CobolElement(cobolName = "REPLY-ADDRESS", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X(20)", srceLine = 72)
    protected String replyAddress;
    @XmlElement(name = "ReplyPhone", required = true)
    @CobolElement(cobolName = "REPLY-PHONE", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X(8)", srceLine = 73)
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
