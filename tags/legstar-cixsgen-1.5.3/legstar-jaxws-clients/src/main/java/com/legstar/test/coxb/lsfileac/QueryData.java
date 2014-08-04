
package com.legstar.test.coxb.lsfileac;

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
 *         &lt;element name="QueryName" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="QueryAddress" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="QueryPhone" type="{http://www.w3.org/2001/XMLSchema}string"/>
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
    "queryName",
    "queryAddress",
    "queryPhone"
})
public class QueryData {

    @XmlElement(name = "QueryName", required = true)
    protected String queryName;
    @XmlElement(name = "QueryAddress", required = true)
    protected String queryAddress;
    @XmlElement(name = "QueryPhone", required = true)
    protected String queryPhone;

    /**
     * Gets the value of the queryName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getQueryName() {
        return queryName;
    }

    /**
     * Sets the value of the queryName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setQueryName(String value) {
        this.queryName = value;
    }

    /**
     * Gets the value of the queryAddress property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getQueryAddress() {
        return queryAddress;
    }

    /**
     * Sets the value of the queryAddress property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setQueryAddress(String value) {
        this.queryAddress = value;
    }

    /**
     * Gets the value of the queryPhone property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getQueryPhone() {
        return queryPhone;
    }

    /**
     * Sets the value of the queryPhone property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setQueryPhone(String value) {
        this.queryPhone = value;
    }

}
