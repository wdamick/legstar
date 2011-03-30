
package com.legstar.test.coxb.lsfileac;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ReplyStatus complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ReplyStatus">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ReplyType" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="SearchDuration" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="TotalItemsRead" type="{http://www.w3.org/2001/XMLSchema}long"/>
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
@XmlType(name = "ReplyStatus", propOrder = {
    "replyType",
    "searchDuration",
    "totalItemsRead",
    "replyResp",
    "replyResp2",
    "replyMessage"
})
public class ReplyStatus {

    @XmlElement(name = "ReplyType")
    protected int replyType;
    @XmlElement(name = "SearchDuration", required = true)
    protected String searchDuration;
    @XmlElement(name = "TotalItemsRead")
    protected long totalItemsRead;
    @XmlElement(name = "ReplyResp")
    protected int replyResp;
    @XmlElement(name = "ReplyResp2")
    protected int replyResp2;
    @XmlElement(name = "ReplyMessage", required = true)
    protected String replyMessage;

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
     * Gets the value of the searchDuration property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSearchDuration() {
        return searchDuration;
    }

    /**
     * Sets the value of the searchDuration property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSearchDuration(String value) {
        this.searchDuration = value;
    }

    /**
     * Gets the value of the totalItemsRead property.
     * 
     */
    public long getTotalItemsRead() {
        return totalItemsRead;
    }

    /**
     * Sets the value of the totalItemsRead property.
     * 
     */
    public void setTotalItemsRead(long value) {
        this.totalItemsRead = value;
    }

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
