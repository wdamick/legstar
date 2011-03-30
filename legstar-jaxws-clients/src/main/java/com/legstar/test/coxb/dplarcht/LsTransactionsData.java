
package com.legstar.test.coxb.dplarcht;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsTransactionsData complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsTransactionsData">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsTransactionName" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="LsTransactionProgram" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="LsTransactionStatus" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="Filler119" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsTransactionsData", propOrder = {
    "lsTransactionName",
    "lsTransactionProgram",
    "lsTransactionStatus",
    "filler119"
})
public class LsTransactionsData {

    @XmlElement(name = "LsTransactionName", required = true)
    protected String lsTransactionName;
    @XmlElement(name = "LsTransactionProgram", required = true)
    protected String lsTransactionProgram;
    @XmlElement(name = "LsTransactionStatus", required = true)
    protected String lsTransactionStatus;
    @XmlElement(name = "Filler119", required = true)
    protected String filler119;

    /**
     * Gets the value of the lsTransactionName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsTransactionName() {
        return lsTransactionName;
    }

    /**
     * Sets the value of the lsTransactionName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsTransactionName(String value) {
        this.lsTransactionName = value;
    }

    /**
     * Gets the value of the lsTransactionProgram property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsTransactionProgram() {
        return lsTransactionProgram;
    }

    /**
     * Sets the value of the lsTransactionProgram property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsTransactionProgram(String value) {
        this.lsTransactionProgram = value;
    }

    /**
     * Gets the value of the lsTransactionStatus property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsTransactionStatus() {
        return lsTransactionStatus;
    }

    /**
     * Sets the value of the lsTransactionStatus property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsTransactionStatus(String value) {
        this.lsTransactionStatus = value;
    }

    /**
     * Gets the value of the filler119 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller119() {
        return filler119;
    }

    /**
     * Sets the value of the filler119 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller119(String value) {
        this.filler119 = value;
    }

}
