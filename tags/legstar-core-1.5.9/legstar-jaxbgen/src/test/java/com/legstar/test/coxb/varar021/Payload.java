
package com.legstar.test.coxb.varar021;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


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
 *         &lt;element name="WechRequestRows">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="3"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WechDynamicResponseRows">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="3"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WechErrorRows">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="3"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WechAdditionalPageKeys">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="1"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="IStaticData" type="{http://legstar.com/test/coxb/varar021}IStaticData" minOccurs="0"/>
 *         &lt;element name="ODynamicData" type="{http://legstar.com/test/coxb/varar021}ODynamicData" maxOccurs="363" minOccurs="0"/>
 *         &lt;element name="WellpointEaiEbsErrorRow" type="{http://legstar.com/test/coxb/varar021}WellpointEaiEbsErrorRow" maxOccurs="99" minOccurs="0"/>
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
public class Payload
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "WechRequestRows")
    @CobolElement(cobolName = "WECH-REQUEST-ROWS", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 3, isODOObject = true, picture = "9(3)", srceLine = 33)
    protected int wechRequestRows;
    @XmlElement(name = "WechDynamicResponseRows")
    @CobolElement(cobolName = "WECH-DYNAMIC-RESPONSE-ROWS", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 3, isODOObject = true, picture = "9(3)", srceLine = 34)
    protected int wechDynamicResponseRows;
    @XmlElement(name = "WechErrorRows")
    @CobolElement(cobolName = "WECH-ERROR-ROWS", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 3, isODOObject = true, picture = "9(3)", srceLine = 35)
    protected int wechErrorRows;
    @XmlElement(name = "WechAdditionalPageKeys", required = true)
    @CobolElement(cobolName = "WECH-ADDITIONAL-PAGE-KEYS", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(1)", srceLine = 36)
    protected String wechAdditionalPageKeys;
    @XmlElement(name = "IStaticData")
    @CobolElement(cobolName = "I-STATIC-DATA", type = CobolType.GROUP_ITEM, levelNumber = 5, minOccurs = 0, maxOccurs = 1, dependingOn = "WECH-REQUEST-ROWS", srceLine = 37)
    protected IStaticData iStaticData;
    @XmlElement(name = "ODynamicData")
    @CobolElement(cobolName = "O-DYNAMIC-DATA", type = CobolType.GROUP_ITEM, levelNumber = 5, minOccurs = 0, maxOccurs = 363, dependingOn = "WECH-DYNAMIC-RESPONSE-ROWS", srceLine = 42)
    protected List<ODynamicData> oDynamicData;
    @XmlElement(name = "WellpointEaiEbsErrorRow")
    @CobolElement(cobolName = "WELLPOINT-EAI-EBS-ERROR-ROW", type = CobolType.GROUP_ITEM, levelNumber = 5, minOccurs = 0, maxOccurs = 99, dependingOn = "WECH-ERROR-ROWS", srceLine = 47)
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

    public boolean isSetWechRequestRows() {
        return true;
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

    public boolean isSetWechDynamicResponseRows() {
        return true;
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

    public boolean isSetWechErrorRows() {
        return true;
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

    public boolean isSetWechAdditionalPageKeys() {
        return (this.wechAdditionalPageKeys!= null);
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

    public boolean isSetIStaticData() {
        return (this.iStaticData!= null);
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

    public boolean isSetODynamicData() {
        return ((this.oDynamicData!= null)&&(!this.oDynamicData.isEmpty()));
    }

    public void unsetODynamicData() {
        this.oDynamicData = null;
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

    public boolean isSetWellpointEaiEbsErrorRow() {
        return ((this.wellpointEaiEbsErrorRow!= null)&&(!this.wellpointEaiEbsErrorRow.isEmpty()));
    }

    public void unsetWellpointEaiEbsErrorRow() {
        this.wellpointEaiEbsErrorRow = null;
    }

}
