
package com.legstar.test.cixs.lsfileam;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsfileamRequest complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsfileamRequest">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{http://cixs.test.legstar.com/lsfileam}LsfileamRequestHolder"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsfileamRequest", propOrder = {
    "lsfileamRequestHolder"
})
public class LsfileamRequest {

    @XmlElement(name = "LsfileamRequestHolder", required = true)
    protected LsfileamRequestHolder lsfileamRequestHolder;

    /**
     * Gets the value of the lsfileamRequestHolder property.
     * 
     * @return
     *     possible object is
     *     {@link LsfileamRequestHolder }
     *     
     */
    public LsfileamRequestHolder getLsfileamRequestHolder() {
        return lsfileamRequestHolder;
    }

    /**
     * Sets the value of the lsfileamRequestHolder property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsfileamRequestHolder }
     *     
     */
    public void setLsfileamRequestHolder(LsfileamRequestHolder value) {
        this.lsfileamRequestHolder = value;
    }

}
