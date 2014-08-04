
package com.legstar.test.coxb.lsfileaq;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for Dfhcommarea complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Dfhcommarea">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="QueryData" type="{http://legstar.com/test/coxb/lsfileaq}QueryData"/>
 *         &lt;element name="ReplyData" type="{http://legstar.com/test/coxb/lsfileaq}ReplyData"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Dfhcommarea", propOrder = {
    "queryData",
    "replyData"
})
public class Dfhcommarea {

    @XmlElement(name = "QueryData", required = true)
    protected QueryData queryData;
    @XmlElement(name = "ReplyData", required = true)
    protected ReplyData replyData;

    /**
     * Gets the value of the queryData property.
     * 
     * @return
     *     possible object is
     *     {@link QueryData }
     *     
     */
    public QueryData getQueryData() {
        return queryData;
    }

    /**
     * Sets the value of the queryData property.
     * 
     * @param value
     *     allowed object is
     *     {@link QueryData }
     *     
     */
    public void setQueryData(QueryData value) {
        this.queryData = value;
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
