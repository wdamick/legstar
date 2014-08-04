
package com.legstar.test.coxb.lsfileaq;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for Customer complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Customer">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CustomerId" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="PersonalData" type="{http://legstar.com/test/coxb/lsfileaq}PersonalData"/>
 *         &lt;element name="LastTransDate" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="Filler49" type="{http://legstar.com/test/coxb/lsfileaq}Filler49" minOccurs="0"/>
 *         &lt;element name="LastTransAmount" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="LastTransComment" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Customer", propOrder = {
    "customerId",
    "personalData",
    "lastTransDate",
    "filler49",
    "lastTransAmount",
    "lastTransComment"
})
public class Customer {

    @XmlElement(name = "CustomerId")
    protected long customerId;
    @XmlElement(name = "PersonalData", required = true)
    protected PersonalData personalData;
    @XmlElement(name = "LastTransDate")
    protected String lastTransDate;
    @XmlElement(name = "Filler49")
    protected Filler49 filler49;
    @XmlElement(name = "LastTransAmount", required = true)
    protected String lastTransAmount;
    @XmlElement(name = "LastTransComment", required = true)
    protected String lastTransComment;

    /**
     * Gets the value of the customerId property.
     * 
     */
    public long getCustomerId() {
        return customerId;
    }

    /**
     * Sets the value of the customerId property.
     * 
     */
    public void setCustomerId(long value) {
        this.customerId = value;
    }

    /**
     * Gets the value of the personalData property.
     * 
     * @return
     *     possible object is
     *     {@link PersonalData }
     *     
     */
    public PersonalData getPersonalData() {
        return personalData;
    }

    /**
     * Sets the value of the personalData property.
     * 
     * @param value
     *     allowed object is
     *     {@link PersonalData }
     *     
     */
    public void setPersonalData(PersonalData value) {
        this.personalData = value;
    }

    /**
     * Gets the value of the lastTransDate property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLastTransDate() {
        return lastTransDate;
    }

    /**
     * Sets the value of the lastTransDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLastTransDate(String value) {
        this.lastTransDate = value;
    }

    /**
     * Gets the value of the filler49 property.
     * 
     * @return
     *     possible object is
     *     {@link Filler49 }
     *     
     */
    public Filler49 getFiller49() {
        return filler49;
    }

    /**
     * Sets the value of the filler49 property.
     * 
     * @param value
     *     allowed object is
     *     {@link Filler49 }
     *     
     */
    public void setFiller49(Filler49 value) {
        this.filler49 = value;
    }

    /**
     * Gets the value of the lastTransAmount property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLastTransAmount() {
        return lastTransAmount;
    }

    /**
     * Sets the value of the lastTransAmount property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLastTransAmount(String value) {
        this.lastTransAmount = value;
    }

    /**
     * Gets the value of the lastTransComment property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLastTransComment() {
        return lastTransComment;
    }

    /**
     * Sets the value of the lastTransComment property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLastTransComment(String value) {
        this.lastTransComment = value;
    }

}
