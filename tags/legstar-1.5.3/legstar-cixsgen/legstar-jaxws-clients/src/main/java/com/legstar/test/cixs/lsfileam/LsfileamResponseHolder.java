
package com.legstar.test.cixs.lsfileam;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.test.coxb.lsfileac.ReplyData;
import com.legstar.test.coxb.lsfileac.ReplyStatus;


/**
 * <p>Java class for LsfileamResponseHolder complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsfileamResponseHolder">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{http://legstar.com/test/coxb/lsfileac}ReplyStatus"/>
 *         &lt;element ref="{http://legstar.com/test/coxb/lsfileac}ReplyData"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsfileamResponseHolder", propOrder = {
    "replyStatus",
    "replyData"
})
public class LsfileamResponseHolder {

    @XmlElement(name = "ReplyStatus", namespace = "http://legstar.com/test/coxb/lsfileac", required = true)
    protected ReplyStatus replyStatus;
    @XmlElement(name = "ReplyData", namespace = "http://legstar.com/test/coxb/lsfileac", required = true)
    protected ReplyData replyData;

    /**
     * Gets the value of the replyStatus property.
     * 
     * @return
     *     possible object is
     *     {@link ReplyStatus }
     *     
     */
    public ReplyStatus getReplyStatus() {
        return replyStatus;
    }

    /**
     * Sets the value of the replyStatus property.
     * 
     * @param value
     *     allowed object is
     *     {@link ReplyStatus }
     *     
     */
    public void setReplyStatus(ReplyStatus value) {
        this.replyStatus = value;
    }

    /**
     * Gets the value of the replyData property.
     * 
     * @return
     *     possible object is
     *     {@link ReplyData }
     *     
     */
    public ReplyData getReplyData() {
        return replyData;
    }

    /**
     * Sets the value of the replyData property.
     * 
     * @param value
     *     allowed object is
     *     {@link ReplyData }
     *     
     */
    public void setReplyData(ReplyData value) {
        this.replyData = value;
    }

}
