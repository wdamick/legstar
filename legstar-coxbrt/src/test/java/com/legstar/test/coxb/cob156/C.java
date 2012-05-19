package com.legstar.test.coxb.cob156;

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
 * Java class for C complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="C">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="d" maxOccurs="500">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="1"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "C", propOrder = { "d" })
public class C implements Serializable {

    private final static long serialVersionUID = 1L;
    @XmlElement(required = true)
    @CobolElement(cobolName = "D", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, minOccurs = 1, maxOccurs = 500, dependingOn = "B", picture = "X", srceLine = 4)
    protected List < String > d;

    /**
     * Gets the value of the d property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the d property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getD().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list {@link String }
     * 
     * 
     */
    public List < String > getD() {
        if (d == null) {
            d = new ArrayList < String >();
        }
        return this.d;
    }

    public boolean isSetD() {
        return ((this.d != null) && (!this.d.isEmpty()));
    }

    public void unsetD() {
        this.d = null;
    }

}
