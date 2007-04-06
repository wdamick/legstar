
package com.legstar.test.coxb.arrayscx;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ElementOneType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ElementOneType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ElementTwo" type="{http://legstar.com/test/coxb/arrayscx}ElementTwoType" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ElementOneType", propOrder = {
    "elementTwo"
})
public class ElementOneType {

    @XmlElement(name = "ElementTwo", required = true)
    protected List<ElementTwoType> elementTwo;

    /**
     * Gets the value of the elementTwo property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the elementTwo property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getElementTwo().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ElementTwoType }
     * 
     * 
     */
    public List<ElementTwoType> getElementTwo() {
        if (elementTwo == null) {
            elementTwo = new ArrayList<ElementTwoType>();
        }
        return this.elementTwo;
    }

}
