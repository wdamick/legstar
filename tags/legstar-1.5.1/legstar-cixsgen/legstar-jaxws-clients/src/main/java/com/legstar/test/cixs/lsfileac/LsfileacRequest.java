
package com.legstar.test.cixs.lsfileac;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsfileacRequest complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsfileacRequest">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{http://cixs.test.legstar.com/lsfileac}LsfileacRequestHolder"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsfileacRequest", propOrder = {
    "lsfileacRequestHolder"
})
public class LsfileacRequest {

    @XmlElement(name = "LsfileacRequestHolder", required = true)
    protected LsfileacRequestHolder lsfileacRequestHolder;

    /**
     * Gets the value of the lsfileacRequestHolder property.
     * 
     * @return
     *     possible object is
     *     {@link LsfileacRequestHolder }
     *     
     */
    public LsfileacRequestHolder getLsfileacRequestHolder() {
        return lsfileacRequestHolder;
    }

    /**
     * Sets the value of the lsfileacRequestHolder property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsfileacRequestHolder }
     *     
     */
    public void setLsfileacRequestHolder(LsfileacRequestHolder value) {
        this.lsfileacRequestHolder = value;
    }

}
