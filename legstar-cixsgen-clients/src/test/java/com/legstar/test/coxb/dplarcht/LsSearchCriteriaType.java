
package com.legstar.test.coxb.dplarcht;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsSearchCriteriaType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsSearchCriteriaType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsStartwith" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="LsStartwithLen" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsSearchCriteriaType", propOrder = {
    "lsStartwith",
    "lsStartwithLen"
})
public class LsSearchCriteriaType {

    @XmlElement(name = "LsStartwith", required = true)
    protected String lsStartwith;
    @XmlElement(name = "LsStartwithLen")
    protected long lsStartwithLen;

    /**
     * Gets the value of the lsStartwith property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsStartwith() {
        return lsStartwith;
    }

    /**
     * Sets the value of the lsStartwith property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsStartwith(String value) {
        this.lsStartwith = value;
    }

    /**
     * Gets the value of the lsStartwithLen property.
     * 
     */
    public long getLsStartwithLen() {
        return lsStartwithLen;
    }

    /**
     * Sets the value of the lsStartwithLen property.
     * 
     */
    public void setLsStartwithLen(long value) {
        this.lsStartwithLen = value;
    }

}
