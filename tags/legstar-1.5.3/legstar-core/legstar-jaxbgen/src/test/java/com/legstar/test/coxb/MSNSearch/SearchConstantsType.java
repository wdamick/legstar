
package com.legstar.test.coxb.MSNSearch;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for SearchConstants complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="SearchConstants">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="MarkBegin" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="MarkEnd" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SearchConstants", propOrder = {
    "markBegin",
    "markEnd"
})
public class SearchConstantsType
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "MarkBegin", defaultValue = "?")
    protected String markBegin;
    @XmlElement(name = "MarkEnd", defaultValue = "?")
    protected String markEnd;

    /**
     * Gets the value of the markBegin property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMarkBegin() {
        return markBegin;
    }

    /**
     * Sets the value of the markBegin property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMarkBegin(String value) {
        this.markBegin = value;
    }

    public boolean isSetMarkBegin() {
        return (this.markBegin!= null);
    }

    /**
     * Gets the value of the markEnd property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMarkEnd() {
        return markEnd;
    }

    /**
     * Sets the value of the markEnd property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMarkEnd(String value) {
        this.markEnd = value;
    }

    public boolean isSetMarkEnd() {
        return (this.markEnd!= null);
    }

}
