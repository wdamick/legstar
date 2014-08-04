
package com.legstar.test.coxb.dplarcht;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsReply complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsReply">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsReplyType" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="LsReplyData" type="{http://legstar.com/test/coxb/dplarcht}LsReplyData"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsReply", propOrder = {
    "lsReplyType",
    "lsReplyData"
})
public class LsReply {

    @XmlElement(name = "LsReplyType")
    protected int lsReplyType;
    @XmlElement(name = "LsReplyData", required = true)
    protected LsReplyData lsReplyData;

    /**
     * Gets the value of the lsReplyType property.
     * 
     */
    public int getLsReplyType() {
        return lsReplyType;
    }

    /**
     * Sets the value of the lsReplyType property.
     * 
     */
    public void setLsReplyType(int value) {
        this.lsReplyType = value;
    }

    /**
     * Gets the value of the lsReplyData property.
     * 
     * @return
     *     possible object is
     *     {@link LsReplyData }
     *     
     */
    public LsReplyData getLsReplyData() {
        return lsReplyData;
    }

    /**
     * Sets the value of the lsReplyData property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsReplyData }
     *     
     */
    public void setLsReplyData(LsReplyData value) {
        this.lsReplyData = value;
    }

}
