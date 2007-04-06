
package com.legstar.test.coxb.vararcom;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for DfhcommareaType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="DfhcommareaType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CItemsNumber" type="{http://www.w3.org/2001/XMLSchema}short"/>
 *         &lt;element name="CArray" type="{http://legstar.com/test/coxb/vararcom}CArrayType" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DfhcommareaType", propOrder = {
    "cItemsNumber",
    "cArray"
})
public class DfhcommareaType {

    @XmlElement(name = "CItemsNumber")
    protected short cItemsNumber;
    @XmlElement(name = "CArray", required = true)
    protected List<CArrayType> cArray;

    /**
     * Gets the value of the cItemsNumber property.
     * 
     */
    public short getCItemsNumber() {
        return cItemsNumber;
    }

    /**
     * Sets the value of the cItemsNumber property.
     * 
     */
    public void setCItemsNumber(short value) {
        this.cItemsNumber = value;
    }

    /**
     * Gets the value of the cArray property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the cArray property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getCArray().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link CArrayType }
     * 
     * 
     */
    public List<CArrayType> getCArray() {
        if (cArray == null) {
            cArray = new ArrayList<CArrayType>();
        }
        return this.cArray;
    }

}
