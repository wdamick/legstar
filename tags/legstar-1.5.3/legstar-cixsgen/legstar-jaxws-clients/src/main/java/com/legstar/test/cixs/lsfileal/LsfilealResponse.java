
package com.legstar.test.cixs.lsfileal;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.test.coxb.lsfileal.ReplyData;


/**
 * <p>Java class for LsfilealResponse complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsfilealResponse">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{http://legstar.com/test/coxb/lsfileal}ReplyData"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsfilealResponse", propOrder = {
    "replyData"
})
public class LsfilealResponse {

    @XmlElement(name = "ReplyData", namespace = "http://legstar.com/test/coxb/lsfileal", required = true)
    protected ReplyData replyData;

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
