
package com.legstar.test.coxb.dplarcht;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


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
 *         &lt;element name="LsItemsCount" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="LsItemsArray" type="{http://legstar.com/test/coxb/dplarcht}LsItemsArrayType" maxOccurs="unbounded"/>
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
    protected long lsItemsCount;
    @XmlElement(name = "LsItemsArray", required = true)
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

}
