
package com.legstar.test.coxb.redinout;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for CParaout complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CParaout">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CSomeOutput" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="Filler27" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CParaout", propOrder = {
    "cSomeOutput",
    "filler27"
})
public class CParaout {

    @XmlElement(name = "CSomeOutput")
    protected long cSomeOutput;
    @XmlElement(name = "Filler27", required = true)
    protected String filler27;

    /**
     * Gets the value of the cSomeOutput property.
     * 
     */
    public long getCSomeOutput() {
        return cSomeOutput;
    }

    /**
     * Sets the value of the cSomeOutput property.
     * 
     */
    public void setCSomeOutput(long value) {
        this.cSomeOutput = value;
    }

    /**
     * Gets the value of the filler27 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller27() {
        return filler27;
    }

    /**
     * Sets the value of the filler27 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller27(String value) {
        this.filler27 = value;
    }

}
