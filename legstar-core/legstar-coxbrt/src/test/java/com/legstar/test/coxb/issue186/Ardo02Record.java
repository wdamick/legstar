package com.legstar.test.coxb.issue186;

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
 * Java class for Ardo02Record complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="Ardo02Record">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;choice>
 *           &lt;element name="AlternativeA" type="{http://legstar.com/test/coxb/ardo02}AlternativeA"/>
 *           &lt;element name="AlternativeB" type="{http://legstar.com/test/coxb/ardo02}AlternativeB"/>
 *         &lt;/choice>
 *         &lt;element name="OdoArray" type="{http://legstar.com/test/coxb/ardo02}OdoArray" maxOccurs="5" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Ardo02Record", propOrder = { "alternativeA", "alternativeB",
        "odoArray" })
public class Ardo02Record implements Serializable {

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "AlternativeA")
    @CobolElement(cobolName = "ALTERNATIVE-A", type = CobolType.GROUP_ITEM, levelNumber = 3, isRedefined = true, srceLine = 5)
    protected AlternativeA alternativeA;
    @XmlElement(name = "AlternativeB")
    @CobolElement(cobolName = "ALTERNATIVE-B", type = CobolType.GROUP_ITEM, levelNumber = 3, redefines = "ALTERNATIVE-A", srceLine = 7)
    protected AlternativeB alternativeB;
    @XmlElement(name = "OdoArray")
    @CobolElement(cobolName = "ODO-ARRAY", type = CobolType.GROUP_ITEM, levelNumber = 3, minOccurs = -1, maxOccurs = 5, dependingOn = "ODO-COUNTER", srceLine = 9)
    protected List < OdoArray > odoArray;

    /**
     * Gets the value of the alternativeA property.
     * 
     * @return possible object is {@link AlternativeA }
     * 
     */
    public AlternativeA getAlternativeA() {
        return alternativeA;
    }

    /**
     * Sets the value of the alternativeA property.
     * 
     * @param value allowed object is {@link AlternativeA }
     * 
     */
    public void setAlternativeA(AlternativeA value) {
        this.alternativeA = value;
    }

    public boolean isSetAlternativeA() {
        return (this.alternativeA != null);
    }

    /**
     * Gets the value of the alternativeB property.
     * 
     * @return possible object is {@link AlternativeB }
     * 
     */
    public AlternativeB getAlternativeB() {
        return alternativeB;
    }

    /**
     * Sets the value of the alternativeB property.
     * 
     * @param value allowed object is {@link AlternativeB }
     * 
     */
    public void setAlternativeB(AlternativeB value) {
        this.alternativeB = value;
    }

    public boolean isSetAlternativeB() {
        return (this.alternativeB != null);
    }

    /**
     * Gets the value of the odoArray property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the odoArray property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getOdoArray().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list {@link OdoArray }
     * 
     * 
     */
    public List < OdoArray > getOdoArray() {
        if (odoArray == null) {
            odoArray = new ArrayList < OdoArray >();
        }
        return this.odoArray;
    }

    public boolean isSetOdoArray() {
        return ((this.odoArray != null) && (!this.odoArray.isEmpty()));
    }

    public void unsetOdoArray() {
        this.odoArray = null;
    }

}
