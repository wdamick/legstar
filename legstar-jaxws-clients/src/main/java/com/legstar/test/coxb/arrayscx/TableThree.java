
package com.legstar.test.coxb.arrayscx;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for TableThree complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="TableThree">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ElementOne" type="{http://legstar.com/test/coxb/arrayscx}ElementOne" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "TableThree", propOrder = {
    "elementOne"
})
public class TableThree {

    @XmlElement(name = "ElementOne", required = true)
    protected List<ElementOne> elementOne;

    /**
     * Gets the value of the elementOne property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the elementOne property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getElementOne().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ElementOne }
     * 
     * 
     */
    public List<ElementOne> getElementOne() {
        if (elementOne == null) {
            elementOne = new ArrayList<ElementOne>();
        }
        return this.elementOne;
    }

}
