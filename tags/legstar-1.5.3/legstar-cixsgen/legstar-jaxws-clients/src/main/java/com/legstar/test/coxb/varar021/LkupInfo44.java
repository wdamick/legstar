
package com.legstar.test.coxb.varar021;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LkupInfo44 complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LkupInfo44">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LkupIdCt" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="LkupTypCdCt" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LkupInfo44", propOrder = {
    "lkupIdCt",
    "lkupTypCdCt"
})
public class LkupInfo44 {

    @XmlElement(name = "LkupIdCt", required = true)
    protected String lkupIdCt;
    @XmlElement(name = "LkupTypCdCt", required = true)
    protected String lkupTypCdCt;

    /**
     * Gets the value of the lkupIdCt property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLkupIdCt() {
        return lkupIdCt;
    }

    /**
     * Sets the value of the lkupIdCt property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLkupIdCt(String value) {
        this.lkupIdCt = value;
    }

    /**
     * Gets the value of the lkupTypCdCt property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLkupTypCdCt() {
        return lkupTypCdCt;
    }

    /**
     * Sets the value of the lkupTypCdCt property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLkupTypCdCt(String value) {
        this.lkupTypCdCt = value;
    }

}
