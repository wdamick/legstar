
package com.legstar.test.coxb.dplarcht;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsItemsArray complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsItemsArray">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsFilesData" type="{http://legstar.com/test/coxb/dplarcht}LsFilesData" minOccurs="0"/>
 *         &lt;element name="LsProgramsData" type="{http://legstar.com/test/coxb/dplarcht}LsProgramsData" minOccurs="0"/>
 *         &lt;element name="LsTransactionsData" type="{http://legstar.com/test/coxb/dplarcht}LsTransactionsData" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsItemsArray", propOrder = {
    "lsFilesData",
    "lsProgramsData",
    "lsTransactionsData"
})
public class LsItemsArray {

    @XmlElement(name = "LsFilesData")
    protected LsFilesData lsFilesData;
    @XmlElement(name = "LsProgramsData")
    protected LsProgramsData lsProgramsData;
    @XmlElement(name = "LsTransactionsData")
    protected LsTransactionsData lsTransactionsData;

    /**
     * Gets the value of the lsFilesData property.
     * 
     * @return
     *     possible object is
     *     {@link LsFilesData }
     *     
     */
    public LsFilesData getLsFilesData() {
        return lsFilesData;
    }

    /**
     * Sets the value of the lsFilesData property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsFilesData }
     *     
     */
    public void setLsFilesData(LsFilesData value) {
        this.lsFilesData = value;
    }

    /**
     * Gets the value of the lsProgramsData property.
     * 
     * @return
     *     possible object is
     *     {@link LsProgramsData }
     *     
     */
    public LsProgramsData getLsProgramsData() {
        return lsProgramsData;
    }

    /**
     * Sets the value of the lsProgramsData property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsProgramsData }
     *     
     */
    public void setLsProgramsData(LsProgramsData value) {
        this.lsProgramsData = value;
    }

    /**
     * Gets the value of the lsTransactionsData property.
     * 
     * @return
     *     possible object is
     *     {@link LsTransactionsData }
     *     
     */
    public LsTransactionsData getLsTransactionsData() {
        return lsTransactionsData;
    }

    /**
     * Sets the value of the lsTransactionsData property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsTransactionsData }
     *     
     */
    public void setLsTransactionsData(LsTransactionsData value) {
        this.lsTransactionsData = value;
    }

}
