package com.legstar.test.coxb.coxb177;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;

/**
 * <p>
 * Java class for Dfhcommarea complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="Dfhcommarea">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;choice>
 *           &lt;element name="spfRecordData">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *                 &lt;maxLength value="12"/>
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
 *           &lt;element name="spfBucketTable" type="{http://coxb.test.legstar.com/athul}SpfBucketTable" maxOccurs="3" minOccurs="3"/>
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
@XmlType(name = "Dfhcommarea", propOrder = { "spfRecordData", "spfBucketTable" })
public class Dfhcommarea implements Serializable {

    private final static long serialVersionUID = 1L;
    @CobolElement(cobolName = "SPF-RECORD-DATA", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, isRedefined = true, picture = "X(12)", srceLine = 2)
    protected String spfRecordData;
    @CobolElement(cobolName = "SPF-BUCKET-TABLE", type = CobolType.GROUP_ITEM, levelNumber = 5, minOccurs = 3, maxOccurs = 3, redefines = "SPF-RECORD-DATA", srceLine = 3)
    protected List < SpfBucketTable > spfBucketTable;

    /**
     * Gets the value of the spfRecordData property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getSpfRecordData() {
        return spfRecordData;
    }

    /**
     * Sets the value of the spfRecordData property.
     * 
     * @param value allowed object is {@link String }
     * 
     */
    public void setSpfRecordData(String value) {
        this.spfRecordData = value;
    }

    public boolean isSetSpfRecordData() {
        return (this.spfRecordData != null);
    }

    /**
     * Gets the value of the spfBucketTable property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the spfBucketTable property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getSpfBucketTable().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link SpfBucketTable }
     * 
     * 
     */
    public List < SpfBucketTable > getSpfBucketTable() {
        if (spfBucketTable == null) {
            spfBucketTable = new ArrayList < SpfBucketTable >();
        }
        return this.spfBucketTable;
    }

    public boolean isSetSpfBucketTable() {
        return ((this.spfBucketTable != null) && (!this.spfBucketTable
                .isEmpty()));
    }

    public void unsetSpfBucketTable() {
        this.spfBucketTable = null;
    }

}
