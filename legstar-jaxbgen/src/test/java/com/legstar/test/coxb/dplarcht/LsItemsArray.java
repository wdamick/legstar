
package com.legstar.test.coxb.dplarcht;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


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
 *         &lt;choice>
 *           &lt;element name="LsFilesData" type="{http://legstar.com/test/coxb/dplarcht}LsFilesData"/>
 *           &lt;element name="LsProgramsData" type="{http://legstar.com/test/coxb/dplarcht}LsProgramsData"/>
 *           &lt;element name="LsTransactionsData" type="{http://legstar.com/test/coxb/dplarcht}LsTransactionsData"/>
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
@XmlType(name = "LsItemsArray", propOrder = {
    "lsFilesData",
    "lsProgramsData",
    "lsTransactionsData"
})
public class LsItemsArray
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "LsFilesData")
    @CobolElement(cobolName = "LS-FILES-DATA", type = CobolType.GROUP_ITEM, levelNumber = 20, isRedefined = true, unmarshalChoiceStrategyClassName = "com.legstar.coxb.cust.dplarcht.ChoiceSelector", srceLine = 102)
    protected LsFilesData lsFilesData;
    @XmlElement(name = "LsProgramsData")
    @CobolElement(cobolName = "LS-PROGRAMS-DATA", type = CobolType.GROUP_ITEM, levelNumber = 20, redefines = "LS-FILES-DATA", srceLine = 107)
    protected LsProgramsData lsProgramsData;
    @XmlElement(name = "LsTransactionsData")
    @CobolElement(cobolName = "LS-TRANSACTIONS-DATA", type = CobolType.GROUP_ITEM, levelNumber = 20, redefines = "LS-FILES-DATA", srceLine = 115)
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

    public boolean isSetLsFilesData() {
        return (this.lsFilesData!= null);
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

    public boolean isSetLsProgramsData() {
        return (this.lsProgramsData!= null);
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

    public boolean isSetLsTransactionsData() {
        return (this.lsTransactionsData!= null);
    }

}
