
package com.legstar.test.cixs.lsfileam;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsfileamResponse complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsfileamResponse">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{http://cixs.test.legstar.com/lsfileam}LsfileamResponseHolder"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsfileamResponse", propOrder = {
    "lsfileamResponseHolder"
})
public class LsfileamResponse {

    @XmlElement(name = "LsfileamResponseHolder", required = true)
    protected LsfileamResponseHolder lsfileamResponseHolder;

    /**
     * Gets the value of the lsfileamResponseHolder property.
     * 
     * @return
     *     possible object is
     *     {@link LsfileamResponseHolder }
     *     
     */
    public LsfileamResponseHolder getLsfileamResponseHolder() {
        return lsfileamResponseHolder;
    }

    /**
     * Sets the value of the lsfileamResponseHolder property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsfileamResponseHolder }
     *     
     */
    public void setLsfileamResponseHolder(LsfileamResponseHolder value) {
        this.lsfileamResponseHolder = value;
    }

}
