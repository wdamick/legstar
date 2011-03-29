
package com.legstar.test.coxb.lsfileac;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for ReplyItem complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ReplyItem">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ReplyNumber">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;totalDigits value="6"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ReplyPersonal" type="{http://legstar.com/test/coxb/lsfileac}ReplyPersonal"/>
 *         &lt;element name="ReplyDate">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ReplyAmount">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ReplyComment">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="9"/>
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
@XmlType(name = "ReplyItem", propOrder = {
    "replyNumber",
    "replyPersonal",
    "replyDate",
    "replyAmount",
    "replyComment"
})
public class ReplyItem
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "ReplyNumber")
    @CobolElement(cobolName = "REPLY-NUMBER", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 15, isSigned = false, totalDigits = 6, picture = "9(6)", srceLine = 67)
    protected long replyNumber;
    @XmlElement(name = "ReplyPersonal", required = true)
    @CobolElement(cobolName = "REPLY-PERSONAL", type = CobolType.GROUP_ITEM, levelNumber = 15, srceLine = 68)
    protected ReplyPersonal replyPersonal;
    @XmlElement(name = "ReplyDate", required = true)
    @CobolElement(cobolName = "REPLY-DATE", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 15, picture = "X(8)", srceLine = 72)
    protected String replyDate;
    @XmlElement(name = "ReplyAmount", required = true)
    @CobolElement(cobolName = "REPLY-AMOUNT", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 15, picture = "X(8)", srceLine = 73)
    protected String replyAmount;
    @XmlElement(name = "ReplyComment", required = true)
    @CobolElement(cobolName = "REPLY-COMMENT", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 15, picture = "X(9)", srceLine = 74)
    protected String replyComment;

    /**
     * Gets the value of the replyNumber property.
     * 
     */
    public long getReplyNumber() {
        return replyNumber;
    }

    /**
     * Sets the value of the replyNumber property.
     * 
     */
    public void setReplyNumber(long value) {
        this.replyNumber = value;
    }

    public boolean isSetReplyNumber() {
        return true;
    }

    /**
     * Gets the value of the replyPersonal property.
     * 
     * @return
     *     possible object is
     *     {@link ReplyPersonal }
     *     
     */
    public ReplyPersonal getReplyPersonal() {
        return replyPersonal;
    }

    /**
     * Sets the value of the replyPersonal property.
     * 
     * @param value
     *     allowed object is
     *     {@link ReplyPersonal }
     *     
     */
    public void setReplyPersonal(ReplyPersonal value) {
        this.replyPersonal = value;
    }

    public boolean isSetReplyPersonal() {
        return (this.replyPersonal!= null);
    }

    /**
     * Gets the value of the replyDate property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReplyDate() {
        return replyDate;
    }

    /**
     * Sets the value of the replyDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReplyDate(String value) {
        this.replyDate = value;
    }

    public boolean isSetReplyDate() {
        return (this.replyDate!= null);
    }

    /**
     * Gets the value of the replyAmount property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReplyAmount() {
        return replyAmount;
    }

    /**
     * Sets the value of the replyAmount property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReplyAmount(String value) {
        this.replyAmount = value;
    }

    public boolean isSetReplyAmount() {
        return (this.replyAmount!= null);
    }

    /**
     * Gets the value of the replyComment property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReplyComment() {
        return replyComment;
    }

    /**
     * Sets the value of the replyComment property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReplyComment(String value) {
        this.replyComment = value;
    }

    public boolean isSetReplyComment() {
        return (this.replyComment!= null);
    }

}
