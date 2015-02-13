package com.legstar.test.coxb.issue187;

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
 * <p>
 * Java class for OdoArray complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="OdoArray">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="OdoSubCounter">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="3"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="OdoSubArray" type="{http://legstar.com/test/coxb/ardo03}OdoSubArray" maxOccurs="5" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "OdoArray", propOrder = { "odoSubCounter", "odoSubArray" })
public class OdoArray implements Serializable {

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "OdoSubCounter")
    @CobolElement(cobolName = "ODO-SUB-COUNTER", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 10, isSigned = false, totalDigits = 3, isODOObject = true, picture = "9(3)", srceLine = 5)
    protected int odoSubCounter;
    @XmlElement(name = "OdoSubArray")
    @CobolElement(cobolName = "ODO-SUB-ARRAY", type = CobolType.GROUP_ITEM, levelNumber = 10, minOccurs = -1, maxOccurs = 5, dependingOn = "ODO-SUB-COUNTER", srceLine = 6)
    protected List < OdoSubArray > odoSubArray;

    /**
     * Gets the value of the odoSubCounter property.
     * 
     */
    public int getOdoSubCounter() {
        return odoSubCounter;
    }

    /**
     * Sets the value of the odoSubCounter property.
     * 
     */
    public void setOdoSubCounter(int value) {
        this.odoSubCounter = value;
    }

    public boolean isSetOdoSubCounter() {
        return true;
    }

    /**
     * Gets the value of the odoSubArray property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the odoSubArray property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getOdoSubArray().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link OdoSubArray }
     * 
     * 
     */
    public List < OdoSubArray > getOdoSubArray() {
        if (odoSubArray == null) {
            odoSubArray = new ArrayList < OdoSubArray >();
        }
        return this.odoSubArray;
    }

    public boolean isSetOdoSubArray() {
        return ((this.odoSubArray != null) && (!this.odoSubArray.isEmpty()));
    }

    public void unsetOdoSubArray() {
        this.odoSubArray = null;
    }

}
