
package com.legstar.test.cixs.lsfileax;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsfileacResponse complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsfileacResponse">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{http://cixs.test.legstar.com/lsfileax}LsfileacResponseHolder"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsfileacResponse", propOrder = {
    "lsfileacResponseHolder"
})
public class LsfileacResponse {

    @XmlElement(name = "LsfileacResponseHolder", required = true)
    protected LsfileacResponseHolder lsfileacResponseHolder;

    /**
     * Gets the value of the lsfileacResponseHolder property.
     * 
     * @return
     *     possible object is
     *     {@link LsfileacResponseHolder }
     *     
     */
    public LsfileacResponseHolder getLsfileacResponseHolder() {
        return lsfileacResponseHolder;
    }

    /**
     * Sets the value of the lsfileacResponseHolder property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsfileacResponseHolder }
     *     
     */
    public void setLsfileacResponseHolder(LsfileacResponseHolder value) {
        this.lsfileacResponseHolder = value;
    }

}
