package com.legstar.test.coxb.pojo156.jaxb;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.legstar.coxb.CobolComplexType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;

/**
 * <p>
 * Java class for someClass complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="someClass">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="items" type="{http://com.legstar.test.coxb/pojo156}someItem" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "someClass", propOrder = { "items" })
@CobolComplexType(javaClassName = "com.legstar.test.coxb.pojo156.SomeClass")
public class SomeClass implements Serializable {

    private final static long serialVersionUID = 1L;
    @XmlElement(nillable = true)
    @CobolElement(cobolName = "items", type = CobolType.GROUP_ITEM, levelNumber = 3, minOccurs = 0, maxOccurs = 10)
    protected List < SomeItem > items;

    /**
     * Gets the value of the items property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the items property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getItems().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list {@link SomeItem }
     * 
     * 
     */
    public List < SomeItem > getItems() {
        if (items == null) {
            items = new ArrayList < SomeItem >();
        }
        return this.items;
    }

    public boolean isSetItems() {
        return ((this.items != null) && (!this.items.isEmpty()));
    }

    public void unsetItems() {
        this.items = null;
    }

}
