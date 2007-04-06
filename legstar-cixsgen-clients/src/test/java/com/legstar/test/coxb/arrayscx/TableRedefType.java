
package com.legstar.test.coxb.arrayscx;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for TableRedefType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="TableRedefType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ElementRedef1" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="ElementRedef2" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "TableRedefType", propOrder = {
    "elementRedef1",
    "elementRedef2"
})
public class TableRedefType {

    @XmlElement(name = "ElementRedef1")
    protected String elementRedef1;
    @XmlElement(name = "ElementRedef2")
    protected Integer elementRedef2;

    /**
     * Gets the value of the elementRedef1 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getElementRedef1() {
        return elementRedef1;
    }

    /**
     * Sets the value of the elementRedef1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setElementRedef1(String value) {
        this.elementRedef1 = value;
    }

    /**
     * Gets the value of the elementRedef2 property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getElementRedef2() {
        return elementRedef2;
    }

    /**
     * Sets the value of the elementRedef2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setElementRedef2(Integer value) {
        this.elementRedef2 = value;
    }

}
