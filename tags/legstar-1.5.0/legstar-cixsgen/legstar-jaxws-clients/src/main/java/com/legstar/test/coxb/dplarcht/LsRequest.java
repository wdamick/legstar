
package com.legstar.test.coxb.dplarcht;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsRequest complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsRequest">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsRequestType" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="LsAllItems" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="LsMaxItems" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/>
 *         &lt;element name="LsSearchCriteria" type="{http://legstar.com/test/coxb/dplarcht}LsSearchCriteria"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsRequest", propOrder = {
    "lsRequestType",
    "lsAllItems",
    "lsMaxItems",
    "lsSearchCriteria"
})
public class LsRequest {

    @XmlElement(name = "LsRequestType")
    protected int lsRequestType;
    @XmlElement(name = "LsAllItems")
    protected String lsAllItems;
    @XmlElement(name = "LsMaxItems")
    protected Integer lsMaxItems;
    @XmlElement(name = "LsSearchCriteria", required = true)
    protected LsSearchCriteria lsSearchCriteria;

    /**
     * Gets the value of the lsRequestType property.
     * 
     */
    public int getLsRequestType() {
        return lsRequestType;
    }

    /**
     * Sets the value of the lsRequestType property.
     * 
     */
    public void setLsRequestType(int value) {
        this.lsRequestType = value;
    }

    /**
     * Gets the value of the lsAllItems property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsAllItems() {
        return lsAllItems;
    }

    /**
     * Sets the value of the lsAllItems property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsAllItems(String value) {
        this.lsAllItems = value;
    }

    /**
     * Gets the value of the lsMaxItems property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getLsMaxItems() {
        return lsMaxItems;
    }

    /**
     * Sets the value of the lsMaxItems property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setLsMaxItems(Integer value) {
        this.lsMaxItems = value;
    }

    /**
     * Gets the value of the lsSearchCriteria property.
     * 
     * @return
     *     possible object is
     *     {@link LsSearchCriteria }
     *     
     */
    public LsSearchCriteria getLsSearchCriteria() {
        return lsSearchCriteria;
    }

    /**
     * Sets the value of the lsSearchCriteria property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsSearchCriteria }
     *     
     */
    public void setLsSearchCriteria(LsSearchCriteria value) {
        this.lsSearchCriteria = value;
    }

}
