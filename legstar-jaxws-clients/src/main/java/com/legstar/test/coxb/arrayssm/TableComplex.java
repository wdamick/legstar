
package com.legstar.test.coxb.arrayssm;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for TableComplex complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="TableComplex">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ElementComplex" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "TableComplex", propOrder = {
    "elementComplex"
})
public class TableComplex {

    @XmlElement(name = "ElementComplex", required = true)
    protected String elementComplex;

    /**
     * Gets the value of the elementComplex property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getElementComplex() {
        return elementComplex;
    }

    /**
     * Sets the value of the elementComplex property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setElementComplex(String value) {
        this.elementComplex = value;
    }

}
