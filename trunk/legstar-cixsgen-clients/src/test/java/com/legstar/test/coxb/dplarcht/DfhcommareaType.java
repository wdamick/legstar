
package com.legstar.test.coxb.dplarcht;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for DfhcommareaType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="DfhcommareaType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsRequest" type="{http://legstar.com/test/coxb/dplarcht}LsRequestType"/>
 *         &lt;element name="LsReply" type="{http://legstar.com/test/coxb/dplarcht}LsReplyType"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DfhcommareaType", propOrder = {
    "lsRequest",
    "lsReply"
})
public class DfhcommareaType {

    @XmlElement(name = "LsRequest", required = true)
    protected LsRequestType lsRequest;
    @XmlElement(name = "LsReply", required = true)
    protected LsReplyType lsReply;

    /**
     * Gets the value of the lsRequest property.
     * 
     * @return
     *     possible object is
     *     {@link LsRequestType }
     *     
     */
    public LsRequestType getLsRequest() {
        return lsRequest;
    }

    /**
     * Sets the value of the lsRequest property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsRequestType }
     *     
     */
    public void setLsRequest(LsRequestType value) {
        this.lsRequest = value;
    }

    /**
     * Gets the value of the lsReply property.
     * 
     * @return
     *     possible object is
     *     {@link LsReplyType }
     *     
     */
    public LsReplyType getLsReply() {
        return lsReply;
    }

    /**
     * Sets the value of the lsReply property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsReplyType }
     *     
     */
    public void setLsReply(LsReplyType value) {
        this.lsReply = value;
    }

}
