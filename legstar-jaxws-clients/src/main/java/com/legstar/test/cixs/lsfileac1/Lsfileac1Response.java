
package com.legstar.test.cixs.lsfileac1;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.test.coxb.lsfileac.ReplyStatus;


/**
 * <p>Java class for Lsfileac1Response complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Lsfileac1Response">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{http://legstar.com/test/coxb/lsfileac}ReplyStatus"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Lsfileac1Response", propOrder = {
    "replyStatus"
})
public class Lsfileac1Response {

    @XmlElement(name = "ReplyStatus", namespace = "http://legstar.com/test/coxb/lsfileac", required = true)
    protected ReplyStatus replyStatus;

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

}
