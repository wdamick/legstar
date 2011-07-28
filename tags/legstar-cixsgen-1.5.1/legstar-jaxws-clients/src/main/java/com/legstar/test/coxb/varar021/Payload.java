
package com.legstar.test.coxb.varar021;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for Payload complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Payload">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="WechRequestRows" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="WechDynamicResponseRows" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="WechErrorRows" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="WechAdditionalPageKeys" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="IStaticData" type="{http://legstar.com/test/coxb/varar021}IStaticData" minOccurs="0"/>
 *         &lt;element name="ODynamicData" type="{http://legstar.com/test/coxb/varar021}ODynamicData" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="WellpointEaiEbsErrorRow" type="{http://legstar.com/test/coxb/varar021}WellpointEaiEbsErrorRow" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Payload", propOrder = {
    "wechRequestRows",
    "wechDynamicResponseRows",
    "wechErrorRows",
    "wechAdditionalPageKeys",
    "iStaticData",
    "oDynamicData",
    "wellpointEaiEbsErrorRow"
})
public class Payload {

    @XmlElement(name = "WechRequestRows")
    protected int wechRequestRows;
    @XmlElement(name = "WechDynamicResponseRows")
    protected int wechDynamicResponseRows;
    @XmlElement(name = "WechErrorRows")
    protected int wechErrorRows;
    @XmlElement(name = "WechAdditionalPageKeys", required = true)
    protected String wechAdditionalPageKeys;
    @XmlElement(name = "IStaticData")
    protected IStaticData iStaticData;
    @XmlElement(name = "ODynamicData")
    protected List<ODynamicData> oDynamicData;
    @XmlElement(name = "WellpointEaiEbsErrorRow")
    protected List<WellpointEaiEbsErrorRow> wellpointEaiEbsErrorRow;

    /**
     * Gets the value of the wechRequestRows property.
     * 
     */
    public int getWechRequestRows() {
        return wechRequestRows;
    }

    /**
     * Sets the value of the wechRequestRows property.
     * 
     */
    public void setWechRequestRows(int value) {
        this.wechRequestRows = value;
    }

    /**
     * Gets the value of the wechDynamicResponseRows property.
     * 
     */
    public int getWechDynamicResponseRows() {
        return wechDynamicResponseRows;
    }

    /**
     * Sets the value of the wechDynamicResponseRows property.
     * 
     */
    public void setWechDynamicResponseRows(int value) {
        this.wechDynamicResponseRows = value;
    }

    /**
     * Gets the value of the wechErrorRows property.
     * 
     */
    public int getWechErrorRows() {
        return wechErrorRows;
    }

    /**
     * Sets the value of the wechErrorRows property.
     * 
     */
    public void setWechErrorRows(int value) {
        this.wechErrorRows = value;
    }

    /**
     * Gets the value of the wechAdditionalPageKeys property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getWechAdditionalPageKeys() {
        return wechAdditionalPageKeys;
    }

    /**
     * Sets the value of the wechAdditionalPageKeys property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setWechAdditionalPageKeys(String value) {
        this.wechAdditionalPageKeys = value;
    }

    /**
     * Gets the value of the iStaticData property.
     * 
     * @return
     *     possible object is
     *     {@link IStaticData }
     *     
     */
    public IStaticData getIStaticData() {
        return iStaticData;
    }

    /**
     * Sets the value of the iStaticData property.
     * 
     * @param value
     *     allowed object is
     *     {@link IStaticData }
     *     
     */
    public void setIStaticData(IStaticData value) {
        this.iStaticData = value;
    }

    /**
     * Gets the value of the oDynamicData property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the oDynamicData property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getODynamicData().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ODynamicData }
     * 
     * 
     */
    public List<ODynamicData> getODynamicData() {
        if (oDynamicData == null) {
            oDynamicData = new ArrayList<ODynamicData>();
        }
        return this.oDynamicData;
    }

    /**
     * Gets the value of the wellpointEaiEbsErrorRow property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the wellpointEaiEbsErrorRow property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getWellpointEaiEbsErrorRow().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link WellpointEaiEbsErrorRow }
     * 
     * 
     */
    public List<WellpointEaiEbsErrorRow> getWellpointEaiEbsErrorRow() {
        if (wellpointEaiEbsErrorRow == null) {
            wellpointEaiEbsErrorRow = new ArrayList<WellpointEaiEbsErrorRow>();
        }
        return this.wellpointEaiEbsErrorRow;
    }

}
