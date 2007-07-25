//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1.3-b01-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.07.17 at 10:43:33 AM CEST 
//


package com.legstar.test.coxb.dplarcht;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for LsItemsArrayType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsItemsArrayType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;choice>
 *           &lt;element name="LsFilesData" type="{http://legstar.com/test/coxb/dplarcht}LsFilesDataType"/>
 *           &lt;element name="LsProgramsData" type="{http://legstar.com/test/coxb/dplarcht}LsProgramsDataType"/>
 *           &lt;element name="LsTransactionsData" type="{http://legstar.com/test/coxb/dplarcht}LsTransactionsDataType"/>
 *         &lt;/choice>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsItemsArrayType", propOrder = {
    "lsFilesData",
    "lsProgramsData",
    "lsTransactionsData"
})
public class LsItemsArrayType {

    @XmlElement(name = "LsFilesData")
    @CobolElement(cobolName = "LS-FILES-DATA", type = CobolType.GROUP_ITEM, levelNumber = 20, isRedefined = true, unmarshalChoiceStrategyClassName = "com.legstar.coxb.cust.dplarcht.ChoiceSelector", srceLine = 102)
    protected LsFilesDataType lsFilesData;
    @XmlElement(name = "LsProgramsData")
    @CobolElement(cobolName = "LS-PROGRAMS-DATA", type = CobolType.GROUP_ITEM, levelNumber = 20, redefines = "LS-FILES-DATA", srceLine = 107)
    protected LsProgramsDataType lsProgramsData;
    @XmlElement(name = "LsTransactionsData")
    @CobolElement(cobolName = "LS-TRANSACTIONS-DATA", type = CobolType.GROUP_ITEM, levelNumber = 20, redefines = "LS-FILES-DATA", srceLine = 115)
    protected LsTransactionsDataType lsTransactionsData;

    /**
     * Gets the value of the lsFilesData property.
     * 
     * @return
     *     possible object is
     *     {@link LsFilesDataType }
     *     
     */
    public LsFilesDataType getLsFilesData() {
        return lsFilesData;
    }

    /**
     * Sets the value of the lsFilesData property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsFilesDataType }
     *     
     */
    public void setLsFilesData(LsFilesDataType value) {
        this.lsFilesData = value;
    }

    public boolean isSetLsFilesData() {
        return (this.lsFilesData!= null);
    }

    /**
     * Gets the value of the lsProgramsData property.
     * 
     * @return
     *     possible object is
     *     {@link LsProgramsDataType }
     *     
     */
    public LsProgramsDataType getLsProgramsData() {
        return lsProgramsData;
    }

    /**
     * Sets the value of the lsProgramsData property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsProgramsDataType }
     *     
     */
    public void setLsProgramsData(LsProgramsDataType value) {
        this.lsProgramsData = value;
    }

    public boolean isSetLsProgramsData() {
        return (this.lsProgramsData!= null);
    }

    /**
     * Gets the value of the lsTransactionsData property.
     * 
     * @return
     *     possible object is
     *     {@link LsTransactionsDataType }
     *     
     */
    public LsTransactionsDataType getLsTransactionsData() {
        return lsTransactionsData;
    }

    /**
     * Sets the value of the lsTransactionsData property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsTransactionsDataType }
     *     
     */
    public void setLsTransactionsData(LsTransactionsDataType value) {
        this.lsTransactionsData = value;
    }

    public boolean isSetLsTransactionsData() {
        return (this.lsTransactionsData!= null);
    }

}
