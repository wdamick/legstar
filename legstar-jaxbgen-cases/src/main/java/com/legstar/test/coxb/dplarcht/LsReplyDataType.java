//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1.3-b01-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.07.13 at 09:00:27 AM CEST 
//


package com.legstar.test.coxb.dplarcht;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for LsReplyDataType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsReplyDataType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsItemsCount">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LsItemsArray" type="{http://legstar.com/test/coxb/dplarcht}LsItemsArrayType" maxOccurs="500"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsReplyDataType", propOrder = {
    "lsItemsCount",
    "lsItemsArray"
})
public class LsReplyDataType {

    @XmlElement(name = "LsItemsCount")
    @CobolElement(cobolName = "LS-ITEMS-COUNT", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, byteLength = 4, isSigned = false, totalDigits = 9, isODOObject = true, picture = "9(9)", usage = "COMP-5", srceLine = 99)
    protected long lsItemsCount;
    @XmlElement(name = "LsItemsArray", required = true)
    @CobolElement(cobolName = "LS-ITEMS-ARRAY", type = CobolType.GROUP_ITEM, levelNumber = 15, minOccurs = 1, maxOccurs = 500, dependingOn = "LS-ITEMS-COUNT", srceLine = 101)
    protected List<LsItemsArrayType> lsItemsArray;

    /**
     * Gets the value of the lsItemsCount property.
     * 
     */
    public long getLsItemsCount() {
        return lsItemsCount;
    }

    /**
     * Sets the value of the lsItemsCount property.
     * 
     */
    public void setLsItemsCount(long value) {
        this.lsItemsCount = value;
    }

    public boolean isSetLsItemsCount() {
        return true;
    }

    /**
     * Gets the value of the lsItemsArray property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the lsItemsArray property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getLsItemsArray().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link LsItemsArrayType }
     * 
     * 
     */
    public List<LsItemsArrayType> getLsItemsArray() {
        if (lsItemsArray == null) {
            lsItemsArray = new ArrayList<LsItemsArrayType>();
        }
        return this.lsItemsArray;
    }

    public boolean isSetLsItemsArray() {
        return ((this.lsItemsArray!= null)&&(!this.lsItemsArray.isEmpty()));
    }

    public void unsetLsItemsArray() {
        this.lsItemsArray = null;
    }

}
