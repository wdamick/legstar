
package com.legstar.test.coxb.redmulti;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for DfhcommareaType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="DfhcommareaType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="COutputType" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="CData" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="Filler35" type="{http://legstar.com/test/coxb/redmulti}Filler35Type" minOccurs="0"/>
 *         &lt;element name="Filler38" type="{http://legstar.com/test/coxb/redmulti}Filler38Type" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DfhcommareaType", propOrder = {
    "cOutputType",
    "cData",
    "filler35",
    "filler38"
})
public class DfhcommareaType {

    @XmlElement(name = "COutputType", required = true)
    protected String cOutputType;
    @XmlElement(name = "CData")
    protected String cData;
    @XmlElement(name = "Filler35")
    protected Filler35Type filler35;
    @XmlElement(name = "Filler38")
    protected Filler38Type filler38;

    /**
     * Gets the value of the cOutputType property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCOutputType() {
        return cOutputType;
    }

    /**
     * Sets the value of the cOutputType property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCOutputType(String value) {
        this.cOutputType = value;
    }

    /**
     * Gets the value of the cData property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCData() {
        return cData;
    }

    /**
     * Sets the value of the cData property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCData(String value) {
        this.cData = value;
    }

    /**
     * Gets the value of the filler35 property.
     * 
     * @return
     *     possible object is
     *     {@link Filler35Type }
     *     
     */
    public Filler35Type getFiller35() {
        return filler35;
    }

    /**
     * Sets the value of the filler35 property.
     * 
     * @param value
     *     allowed object is
     *     {@link Filler35Type }
     *     
     */
    public void setFiller35(Filler35Type value) {
        this.filler35 = value;
    }

    /**
     * Gets the value of the filler38 property.
     * 
     * @return
     *     possible object is
     *     {@link Filler38Type }
     *     
     */
    public Filler38Type getFiller38() {
        return filler38;
    }

    /**
     * Sets the value of the filler38 property.
     * 
     * @param value
     *     allowed object is
     *     {@link Filler38Type }
     *     
     */
    public void setFiller38(Filler38Type value) {
        this.filler38 = value;
    }

}
