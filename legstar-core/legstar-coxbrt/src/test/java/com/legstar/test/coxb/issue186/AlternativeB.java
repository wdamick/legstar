package com.legstar.test.coxb.issue186;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;

/**
 * <p>
 * Java class for AlternativeB complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="AlternativeB">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Filler8">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="2"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "AlternativeB", propOrder = { "filler8" })
public class AlternativeB implements Serializable {

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "Filler8", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "XX", srceLine = 8)
    protected String filler8;

    /**
     * Gets the value of the filler8 property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getFiller8() {
        return filler8;
    }

    /**
     * Sets the value of the filler8 property.
     * 
     * @param value allowed object is {@link String }
     * 
     */
    public void setFiller8(String value) {
        this.filler8 = value;
    }

    public boolean isSetFiller8() {
        return (this.filler8 != null);
    }

}
