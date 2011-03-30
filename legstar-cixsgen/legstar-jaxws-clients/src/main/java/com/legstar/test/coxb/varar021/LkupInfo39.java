
package com.legstar.test.coxb.varar021;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LkupInfo39 complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LkupInfo39">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LkupId" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="LkupTypCd" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LkupInfo39", propOrder = {
    "lkupId",
    "lkupTypCd"
})
public class LkupInfo39 {

    @XmlElement(name = "LkupId", required = true)
    protected String lkupId;
    @XmlElement(name = "LkupTypCd", required = true)
    protected String lkupTypCd;

    /**
     * Gets the value of the lkupId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLkupId() {
        return lkupId;
    }

    /**
     * Sets the value of the lkupId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLkupId(String value) {
        this.lkupId = value;
    }

    /**
     * Gets the value of the lkupTypCd property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLkupTypCd() {
        return lkupTypCd;
    }

    /**
     * Sets the value of the lkupTypCd property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLkupTypCd(String value) {
        this.lkupTypCd = value;
    }

}
