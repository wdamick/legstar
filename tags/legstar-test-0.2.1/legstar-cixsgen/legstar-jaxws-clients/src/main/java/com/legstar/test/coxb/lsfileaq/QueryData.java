
package com.legstar.test.coxb.lsfileaq;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for QueryData complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="QueryData">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CustomerName" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="MaxReplies" type="{http://www.w3.org/2001/XMLSchema}short"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "QueryData", propOrder = {
    "customerName",
    "maxReplies"
})
public class QueryData {

    @XmlElement(name = "CustomerName", required = true)
    protected String customerName;
    @XmlElement(name = "MaxReplies")
    protected short maxReplies;

    /**
     * Gets the value of the customerName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCustomerName() {
        return customerName;
    }

    /**
     * Sets the value of the customerName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCustomerName(String value) {
        this.customerName = value;
    }

    /**
     * Gets the value of the maxReplies property.
     * 
     */
    public short getMaxReplies() {
        return maxReplies;
    }

    /**
     * Sets the value of the maxReplies property.
     * 
     */
    public void setMaxReplies(short value) {
        this.maxReplies = value;
    }

}
